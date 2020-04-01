#version 330

uniform float iTime;
uniform int iFrame;
uniform float iDeltaTime;
uniform vec2 iMousePos;
uniform vec2 iResolution;
uniform int bufferId;

uniform sampler2D buffer0;
uniform sampler2D buffer1;

float inf = 1.0/0.0;

in vec2 uv;
out vec4 fragColor;

struct ray {
        vec3 origin;
        vec3 direction;
};

struct material {
        vec3 color;
};

material black = material(vec3(0));

struct hit {
        bool didHit;
        float distance;
        vec3 normal;
        material mat;
};

hit noHit = hit(false, inf, vec3(0.0), material(black));

hit intersectSphere(ray r, vec3 centre, float radius, material m) {
        vec3 ro = r.origin - centre;
        vec3 rd = r.direction;
        float b = 2.0*dot(rd, ro);
        float c = dot(ro, ro) - radius*radius;
        float root = sqrt(b*b - 4*c);
        float t0 = -b + root/2.0;
        float t1 = -b - root/2.0;
        float t = inf;
        int hits = 0;
        if (t0 > 0) { t = t0; hits++; }
        if (t1 > 0) { t = min(t, t1); hits++; }
        if (hits > 0) {
                vec3 hitPos = r.origin + rd*t;
                vec3 norm = normalize(hitPos - centre);
                return hit(true, t, norm, m);
        } else {
                return noHit;
        }
}

hit union(hit a, hit b) {
        return (a.distance < b.distance ? a : b);
}

hit intersect(ray r) {
        hit sphere1 = intersectSphere(r, vec3(0,0,0), 1.0, material(vec3(1,0,0)));
        hit sphere2 = intersectSphere(r, vec3(1,0,-sin(iTime)), 0.8, material(vec3(0,1,0)));
        return union(sphere1, sphere2);
}

vec3 fireRay(ray r) {
        hit h = intersect(r);
        if (h.didHit) {
                return vec3(h.mat.color * (dot(h.normal, vec3(0, 1, 0))*0.5+0.5));
        } else {
                return vec3(0.0);
        }
}

vec3 renderFrame(void) {
        vec3 camPos = vec3(0, 1, -5);
        vec3 lookAt = vec3(0,0,0);
        vec3 worldUp = vec3(0, 1, 0);
        vec3 camForward = normalize(lookAt - camPos);
        vec3 camRight = cross(camForward, worldUp);
        vec3 camUp = cross(camRight, camForward);
        vec3 filmCentre = camPos + camForward*0.8;
        vec2 filmSize = vec2(iResolution.x/iResolution.y, 1.0);
        vec3 filmPos = filmCentre + camRight*uv.x*filmSize.x + camUp*uv.y*filmSize.y;
        vec3 rd = normalize(filmPos - camPos);
        ray r = ray(camPos, rd);
        return fireRay(r);
}

void main(void) {
        if (bufferId == 0) {
                vec3 c = renderFrame();
                vec4 prev = vec4(0);//texture(buffer0, gl_FragCoord.xy / iResolution);
                fragColor = prev + vec4(c, 0.0);
        } else {
                //fragColor = texture(buffer0, gl_FragCoord.xy / iResolution) / float(iFrame);
                fragColor = texture(buffer0, gl_FragCoord.xy / iResolution);
        }
}
