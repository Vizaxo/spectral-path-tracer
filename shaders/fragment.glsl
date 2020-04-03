#version 330

uniform float iTime;
uniform int iFrame;
uniform float iDeltaTime;
uniform vec2 iMousePos;
uniform vec2 iResolution;
uniform int bufferId;

uniform sampler2D texture0;
uniform sampler2D buffer0;
uniform sampler2D buffer1;

in vec2 uv;
out vec4 fragColor;

////////////////////////////////////////////////////////////////////////////////
// Types
////////////////////////////////////////////////////////////////////////////////

struct ray {
        vec3 origin;
        vec3 direction;
        float wavelength;
};

struct refractiveIndex {
        float coeffA;
        float coeffB;
};

struct material {
        uint matType;
        uint color;
        refractiveIndex ri; // only if matType == transparent
};

// matType enum
#define mirror 1u
#define light 2u
#define diffuse 3u
#define transparent 4u

// color enum
#define white 0u
#define lightGrey 1u
#define darkGrey 2u
#define black 3u
#define red 4u
#define green 5u
#define blue 6u
#define yellow 7u

struct hit {
        bool didHit;
        float distance;
        vec3 hitPos;
        vec3 normal;
        bool insideObj;
        material mat;
};

////////////////////////////////////////////////////////////////////////////////
// Constants
////////////////////////////////////////////////////////////////////////////////

float inf = 1.0/0.0;
float pi = 3.1415926535;
float tau = 2.0*pi;

uint maxBounces = 10u;
float epsilon = 0.001;
uint raysPerPixel = 10u;
uint numRands = 6u;

refractiveIndex riGlass = refractiveIndex(1.4580, 0.00354); //fused silica
refractiveIndex noRi = refractiveIndex(0 ,0);
material blankMaterial = material(diffuse, yellow, noRi);
hit noHit = hit(false, inf, vec3(0.0), vec3(0.0), false, blankMaterial);

////////////////////////////////////////////////////////////////////////////////
// Random sampling
////////////////////////////////////////////////////////////////////////////////

float getRand(uint a, uint randId) {
    a *= numRands;
    a += randId;
    return texelFetch(texture0, ivec2(a%1021u, (a/1019u) % 1024u), 0).r;
}

vec3 sampleSphere(uint seed) {
    float u1 = getRand(seed, 0u);
    float u2 = getRand(seed, 1u);
    float r = sqrt(1. - u1*u1);
    float phi = tau * u2;
    return vec3(cos(phi) * r, sin(phi) * r, 2. * u1);
}

vec3 sampleHemisphere(vec3 normal, uint seed) {
    vec3 v = sampleSphere(seed);
    if (dot(normal, v) < 0.)
        return -v;
    else
        return v;
}

////////////////////////////////////////////////////////////////////////////////
// Light and colour
////////////////////////////////////////////////////////////////////////////////

vec4 fromLinear(vec4 linearRGB)
{
    bvec4 cutoff = lessThan(linearRGB, vec4(0.0031308));
    vec4 higher = vec4(1.055)*pow(linearRGB, vec4(1.0/2.4)) - vec4(0.055);
    vec4 lower = linearRGB * vec4(12.92);

    return mix(higher, lower, cutoff);
}

// Convert a wavelength to CIE color space
vec3 cie_1964(float lambda) {
    switch(int(lambda)/10) {
        case 36: return vec3(0.000000122200, 0.000000013398, 0.000000535027);
        case 37: return vec3(0.000005958600, 0.000000651100, 0.000026143700);
        case 38: return vec3(0.000159952000, 0.000017364000, 0.000704776000);
        case 39: return vec3(0.002361600000, 0.000253400000, 0.010482200000);
        case 40: return vec3(0.019109700000, 0.002004400000, 0.086010900000);
        case 41: return vec3(0.084736000000, 0.008756000000, 0.389366000000);
        case 42: return vec3(0.204492000000, 0.021391000000, 0.972542000000);
        case 43: return vec3(0.314679000000, 0.038676000000, 1.553480000000);
        case 44: return vec3(0.383734000000, 0.062077000000, 1.967280000000);
        case 45: return vec3(0.370702000000, 0.089456000000, 1.994800000000);
        case 46: return vec3(0.302273000000, 0.128201000000, 1.745370000000);
        case 47: return vec3(0.195618000000, 0.185190000000, 1.317560000000);
        case 48: return vec3(0.080507000000, 0.253589000000, 0.772125000000);
        case 49: return vec3(0.016172000000, 0.339133000000, 0.415254000000);
        case 50: return vec3(0.003816000000, 0.460777000000, 0.218502000000);
        case 51: return vec3(0.037465000000, 0.606741000000, 0.112044000000);
        case 52: return vec3(0.117749000000, 0.761757000000, 0.060709000000);
        case 53: return vec3(0.236491000000, 0.875211000000, 0.030451000000);
        case 54: return vec3(0.376772000000, 0.961988000000, 0.013676000000);
        case 55: return vec3(0.529826000000, 0.991761000000, 0.003988000000);
        case 56: return vec3(0.705224000000, 0.997340000000, 0.000000000000);
        case 57: return vec3(0.878655000000, 0.955552000000, 0.000000000000);
        case 58: return vec3(1.014160000000, 0.868934000000, 0.000000000000);
        case 59: return vec3(1.118520000000, 0.777405000000, 0.000000000000);
        case 60: return vec3(1.123990000000, 0.658341000000, 0.000000000000);
        case 61: return vec3(1.030480000000, 0.527963000000, 0.000000000000);
        case 62: return vec3(0.856297000000, 0.398057000000, 0.000000000000);
        case 63: return vec3(0.647467000000, 0.283493000000, 0.000000000000);
        case 64: return vec3(0.431567000000, 0.179828000000, 0.000000000000);
        case 65: return vec3(0.268329000000, 0.107633000000, 0.000000000000);
        case 66: return vec3(0.152568000000, 0.060281000000, 0.000000000000);
        case 67: return vec3(0.081260600000, 0.031800400000, 0.000000000000);
        case 68: return vec3(0.040850800000, 0.015905100000, 0.000000000000);
        case 69: return vec3(0.019941300000, 0.007748800000, 0.000000000000);
        case 70: return vec3(0.009576880000, 0.003717740000, 0.000000000000);
        case 71: return vec3(0.004552630000, 0.001768470000, 0.000000000000);
        case 72: return vec3(0.002174960000, 0.000846190000, 0.000000000000);
        case 73: return vec3(0.001044760000, 0.000407410000, 0.000000000000);
        case 74: return vec3(0.000508258000, 0.000198730000, 0.000000000000);
        case 75: return vec3(0.000250969000, 0.000098428000, 0.000000000000);
        case 76: return vec3(0.000126390000, 0.000049737000, 0.000000000000);
        case 77: return vec3(0.000064525800, 0.000025486000, 0.000000000000);
        case 78: return vec3(0.000033411700, 0.000013249000, 0.000000000000);
        case 79: return vec3(0.000017611500, 0.000007012800, 0.000000000000);
        case 80: return vec3(0.000009413630, 0.000003764730, 0.000000000000);
        case 81: return vec3(0.000005093470, 0.000002046130, 0.000000000000);
        case 82: return vec3(0.000002795310, 0.000001128090, 0.000000000000);
        case 83: return vec3(0.000001553140, 0.000000629700, 0.000000000000);
        default: return vec3(0.0);
    }
}

// Convert a wavelength to linear RGB values
vec3 wavelengthToRGB(float lambda) {
    vec3 xyz = cie_1964(lambda);
    float x = xyz.x;
    float y = xyz.y;
    float z = xyz.z;

    vec3 rgb;
    rgb.r =  3.2404542*x - 1.5371385*y - 0.4985314*z;
	rgb.g = -0.9692660*x + 1.8760108*y + 0.0415560*z;
	rgb.b =  0.0556434*x - 0.2040259*y + 1.0572252*z;
    return rgb;
}

// Get the intensity of a color at the given wavelength
float getIntensity(uint color, float wavelength) {
    vec3 rgb = wavelengthToRGB(wavelength);
    switch (color) {
        case white: return 1.0;
        case lightGrey: return 0.8;
        case darkGrey: return 0.3;
        case black: return 0.0;
        case red: return rgb.r + 0.05*rgb.b + 0.05*rgb.g;
        case green: return rgb.g + 0.05*rgb.b + 0.1*rgb.r;
        case blue: return rgb.b + 0.05*rgb.r + 0.1*rgb.g;
        case yellow: return rgb.g + 0.05*rgb.b + 0.5*rgb.r;
        default: return 0.0;
    }
}

// Cauchy's equation
float getRI(material m, float wavelength) {
    float a = m.ri.coeffA;
    float b = m.ri.coeffA;
    return a + b / (wavelength*wavelength);
}

float schlick(float n1, float n2, float cosTheta) {
    float tmp = (n1-n2)/(n1+n2);
    float r0 = tmp*tmp;
    return r0 + (1.0-r0) * pow(1.0-cosTheta, 5.0);
}

////////////////////////////////////////////////////////////////////////////////
// Intersections
////////////////////////////////////////////////////////////////////////////////

hit intersectSphere(ray r, vec3 centre, float radius, material m) {
        vec3 ro = r.origin - centre;
        vec3 rd = r.direction;
        float b = 2.0*dot(rd, ro);
        float c = dot(ro, ro) - radius*radius;
        float root = sqrt(b*b - 4*c);
        float t0 = (-b + root)/2.0;
        float t1 = (-b - root)/2.0;
        float t = inf;
        if (t0 > 0 && t1 > 0) {
                t = min(t0, t1);
        } else if (t0 > 0) {
                t = t0;
        } else if (t1 > 0) {
                t = t1;
        } else {
                return noHit;
        }
        vec3 hitPos = r.origin + rd*t;
        vec3 norm = normalize(hitPos - centre);
        bool insideObj = dot(norm, -rd) < 0;
        norm = insideObj ? -norm : norm;
        return hit(true, t, hitPos, norm, insideObj, m);
}

hit unionObj(hit a, hit b) {
        return (a.distance < b.distance ? a : b);
}

hit intersect(ray r) {
        hit ground = intersectSphere(r, vec3(0,-200,0), 200.0,
                                     material(diffuse, white, noRi));
        hit ceiling = intersectSphere(r, vec3(0,205,0), 200.0,
                                      material(diffuse, white, noRi));
        hit left = intersectSphere(r, vec3(-205,0,0), 200.0,
                                      material(diffuse, red, noRi));
        hit right = intersectSphere(r, vec3(205,0,0), 200.0,
                                      material(diffuse, green, noRi));
        hit back = intersectSphere(r, vec3(0,0,205), 200.0,
                                      material(diffuse, blue, noRi));
        hit front = intersectSphere(r, vec3(0,0,-205), 200.0,
                                      material(diffuse, black, noRi));

        hit walls = unionObj(unionObj(ground, ceiling),
                             unionObj(unionObj(left, right),
                                      unionObj(back, front)));
        hit glassSphere = intersectSphere(r, vec3(1,1.5,0), 1.0,
                                          material(transparent, white, riGlass));
        hit mirrorSphere = intersectSphere(r, vec3(-2, 0, 0), 1.0,
                                           material(mirror, lightGrey, noRi));
        hit ceilLight = intersectSphere(r, vec3(0,200,0), 195.001,
                                        material(light, white, noRi));
        hit objects = unionObj(ceilLight, unionObj(glassSphere, mirrorSphere));
        return unionObj(walls, objects);
}

////////////////////////////////////////////////////////////////////////////////
// Rendering
////////////////////////////////////////////////////////////////////////////////

float fireRay(ray r, uint seed) {
        float intensity = 1.0;
        for (uint i = 0u; i < maxBounces; i++) {
                hit h = intersect(r);
                if (h.didHit) {
                        r.origin = h.hitPos;
                        switch(h.mat.matType) {
                        case mirror:
                                r.direction = reflect(r.direction, h.normal);
                                break;
                        case light:
                                intensity *= 10.0 * getIntensity(h.mat.color, r.wavelength);
                                return intensity;
                        case diffuse:
                                r.direction = sampleHemisphere(h.normal, seed);
                                intensity *= 1/pi;
                                break;
                        case transparent:
                                float n1, n2;
                                if (h.insideObj) {
                                        n1 = getRI(h.mat, r.wavelength);
                                        n2 = 1.0;
                                } else {
                                        n1 = 1.0;
                                        n2 = getRI(h.mat, r.wavelength);
                                }

                                float u = getRand(seed, 2u);
                                float pReflect = schlick(n1, n2,-(dot(h.normal, r.direction)));
                                if (u < pReflect) {
                                        r.direction = reflect(r.direction, h.normal);
                                } else {
                                        r.direction = refract(r.direction, h.normal, n1/n2);
                                }
                                break;
                        }
                        intensity *= getIntensity(h.mat.color, r.wavelength)
                                * abs(dot(r.direction, h.normal)); //cosine attenuation
                        r.origin += r.direction*epsilon;
                } else {
                        // No hit
                        return 0.0;
                }
        }
        // Exceeded max bounces
        return 0.0;
}

vec3 renderFrame(void) {
        vec3 camPos = vec3(2.0*sin(iMousePos.x/iResolution.x*2*pi), 1 + iMousePos.y/iResolution.y*2.0, 2.0*cos(iMousePos.x/iResolution.x*2*pi));
        vec3 lookAt = vec3(0,1.4,0);
        vec3 worldUp = vec3(0, 1, 0);
        vec3 camForward = normalize(lookAt - camPos);
        vec3 camRight = cross(camForward, worldUp);
        vec3 camUp = cross(camRight, camForward);
        vec3 filmCentre = camPos + camForward*0.6;
        vec2 filmSize = vec2(1.0, iResolution.y/iResolution.x);

        uvec2 res = uvec2(iResolution);
        uint x = uint(gl_FragCoord.x);
        uint y = uint(gl_FragCoord.y);
        uint baseSeed = uint(iTime * 1/iDeltaTime)*res.x*res.y + y*res.x+ x;

        vec3 c = vec3(0);
        for (uint i = 0u; i  < raysPerPixel; i++) {
                uint seed = baseSeed*raysPerPixel + i;
                vec2 jitteredCoord = gl_FragCoord.xy + vec2(getRand(seed, 3u), getRand(seed, 4u)) - 0.5;
                vec2 uv = (jitteredCoord / iResolution)*2.0-1.0;
                vec3 filmPos = filmCentre + camRight*uv.x*filmSize.x + camUp*uv.y*filmSize.y;
                vec3 rd = normalize(filmPos - camPos);

                float u = getRand(seed, 5u);
                float wavelength = mix(380.0, 700.0, u);

                ray r = ray(camPos, rd, wavelength);
                float intensity = fireRay(r, seed);
                c += intensity*wavelengthToRGB(wavelength);
        }
        return c / float(raysPerPixel);
}

void main(void) {
        if (bufferId == 0) {
                vec3 c = renderFrame();
                vec4 prev = texture(buffer0, gl_FragCoord.xy / iResolution);
                fragColor = prev + vec4(c, 0.0);
        } else {
                fragColor = fromLinear(texture(buffer0, gl_FragCoord.xy / iResolution) / float(iFrame));
        }
}
