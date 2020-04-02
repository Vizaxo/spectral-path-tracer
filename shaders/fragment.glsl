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
};


struct material {
        uint matType;
        vec3 color;
};

// matType enum
#define mirror 1u
#define light 2u
#define diffuse 3u

struct hit {
        bool didHit;
        float distance;
        vec3 hitPos;
        vec3 normal;
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
uint numRands = 2u;

material black = material(mirror, vec3(0));
hit noHit = hit(false, inf, vec3(0.0), vec3(0.0), material(black));

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

////////////////////////////////////////////////////////////////////////////////
// Intecsections
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
        int hits = 0;
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
        return hit(true, t, hitPos, norm, m);
}

hit unionObj(hit a, hit b) {
        return (a.distance < b.distance ? a : b);
}

hit intersect(ray r) {
        hit ground = intersectSphere(r, vec3(0,-200,0), 200.0,
                                     material(diffuse, vec3(0.9, 0.9, 0.9)));
        hit ceiling = intersectSphere(r, vec3(0,205,0), 200.0,
                                      material(diffuse, vec3(0.9, 0.9, 0.9)));
        hit left = intersectSphere(r, vec3(-205,0,0), 200.0,
                                      material(diffuse, vec3(0.9, 0.2, 0.2)));
        hit right = intersectSphere(r, vec3(205,0,0), 200.0,
                                      material(diffuse, vec3(0.2, 0.9, 0.2)));
        hit back = intersectSphere(r, vec3(0,0,205), 200.0,
                                      material(diffuse, vec3(0.2, 0.2, 0.9)));
        hit front = intersectSphere(r, vec3(0,0,-205), 200.0,
                                      material(diffuse, vec3(0.2, 0.2, 0.2)));

        hit walls = unionObj(unionObj(ground, ceiling),
                             unionObj(unionObj(left, right),
                                      unionObj(back, front)));
        hit sphere2 = intersectSphere(r, vec3(0,200,0), 195.001,
                                      material(light, vec3(1.0, 1.0, 1.0)));
        hit sphere3 = intersectSphere(r, vec3(-2, 0, 0), 1.0,
                                      material(mirror, vec3(0.6, 0.6, 0.6)));
        return unionObj(walls, (unionObj(sphere2, sphere3)));
}

////////////////////////////////////////////////////////////////////////////////
// Rendering
////////////////////////////////////////////////////////////////////////////////

vec3 fireRay(ray r, uint seed) {
        vec3 color = vec3(1.0);
        for (uint i = 0u; i < maxBounces; i++) {
                hit h = intersect(r);
                if (h.didHit) {
                        r.origin = h.hitPos;
                        switch(h.mat.matType) {
                        case mirror:
                                r.direction = reflect(r.direction, h.normal);
                                break;
                        case light:
                                color *= 2.0 * vec3(h.mat.color);
                                return color;
                        case diffuse:
                                r.direction = sampleHemisphere(h.normal, seed);
                                break;
                        }
                        color *= vec3(h.mat.color) * dot(r.direction, h.normal);
                        r.origin += r.direction*epsilon;
                } else {
                        // No hit
                        return vec3(0.0);
                }
        }
        // Exceeded max bounces
        return vec3(0);
}

vec3 renderFrame(void) {
        vec3 camPos = vec3(3.0*sin(iTime/4.0), 2, 3.0*cos(iTime/4.0));
        vec3 lookAt = vec3(0,1.4,0);
        vec3 worldUp = vec3(0, 1, 0);
        vec3 camForward = normalize(lookAt - camPos);
        vec3 camRight = cross(camForward, worldUp);
        vec3 camUp = cross(camRight, camForward);
        vec3 filmCentre = camPos + camForward*0.6;
        vec2 filmSize = vec2(1.0, iResolution.y/iResolution.x);
        vec3 filmPos = filmCentre + camRight*uv.x*filmSize.x + camUp*uv.y*filmSize.y;
        vec3 rd = normalize(filmPos - camPos);
        ray r = ray(camPos, rd);

        uvec2 res = uvec2(iResolution);
        uint x = uint(gl_FragCoord.x);
        uint y = uint(gl_FragCoord.y);
        uint baseSeed = uint(iFrame)*res.x*res.y + y*res.x+ x;

        vec3 c = vec3(0);
        for (uint i = 0u; i  < raysPerPixel; i++) {
                uint seed = baseSeed*raysPerPixel + i;
                vec2 jitteredCoord = gl_FragCoord.xy + vec2(getRand(seed, 2u), getRand(seed, 3u)) - 0.5;
                vec2 uv = jitteredCoord / iResolution;
                vec3 filmPos = filmCentre + camRight*uv.x*filmSize.x + camUp*uv.y*filmSize.y;
                c += fireRay(r, seed);
        }
        return c / float(raysPerPixel);
}

void main(void) {
        if (bufferId == 0) {
                vec3 c = renderFrame();
                vec4 prev = vec4(0);//texture(buffer0, gl_FragCoord.xy / iResolution);
                fragColor = prev + vec4(c, 0.0);
        } else {
                //fragColor = texture(buffer0, gl_FragCoord.xy / iResolution) / float(iFrame);
                fragColor = fromLinear(texture(buffer0, gl_FragCoord.xy / iResolution));
        }
}
