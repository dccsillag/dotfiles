#version 130
uniform float opacity;
uniform sampler2D tex;
uniform bool invert_color;

vec3 rgb2hsv(vec3 c) {
    /* Taken from https://stackoverflow.com/a/17897228/4803382 */
    vec4 K = vec4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
    vec4 p = mix(vec4(c.bg, K.wz), vec4(c.gb, K.xy), step(c.b, c.g));
    vec4 q = mix(vec4(p.xyw, c.r), vec4(c.r, p.yzx), step(p.x, c.r));

    float d = q.x - min(q.w, q.y);
    float e = 1.0e-10;
    return vec3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}

vec3 hsv2rgb(vec3 c) {
    /* Taken from https://stackoverflow.com/a/17897228/4803382 */
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

float square(float x) {
    return x*x;
}

void main() {
    vec4 c_rgb = texture2D(tex, gl_TexCoord[0].xy);
    vec3 c_hsv = rgb2hsv(c_rgb.xyz); // .x = hue, .y = saturation, .z = value

    //if (invert_color) {
    //    /* c_hsv.z = clamp(c_rgb.a - c_hsv.z, 0.0, 1.0); */
    //    /* c_rgb.xyz = hsv2rgb(c_hsv); */
    //    c_rgb = vec4(vec3(c_rgb.a, c_rgb.a, c_rgb.a) - vec3(c_rgb), c_rgb.a);
    //}

    c_rgb *= mix(opacity, 1.0, clamp(square(2.5*c_hsv.z), 0.0, 1.0));

    gl_FragColor = c_rgb;
}
