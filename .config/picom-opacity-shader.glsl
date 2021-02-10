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

vec2 coord_mod(vec2 coord, float x_axis, float y_axis) {
    return vec2(clamp(coord.x + x_axis, 0, 1), clamp(coord.y + y_axis, 0, 1));
}

void main() {
    vec2 texture_size = textureSize(tex, 0);
    float xstep = 1 / texture_size.x;
    float ystep = 1 / texture_size.y;
    vec4 c_rgb   = texture2D(tex, coord_mod(gl_TexCoord[0].xy, 0,      0));
    vec4 c_rgb_l = texture2D(tex, coord_mod(gl_TexCoord[0].xy, -xstep, 0));
    vec4 c_rgb_r = texture2D(tex, coord_mod(gl_TexCoord[0].xy, xstep,  0));
    vec4 c_rgb_u = texture2D(tex, coord_mod(gl_TexCoord[0].xy, 0,      -ystep));
    vec4 c_rgb_d = texture2D(tex, coord_mod(gl_TexCoord[0].xy, 0,      ystep));
    /* vec4 c_mix = min(min(min(min(c_rgb_l, c_rgb_r), c_rgb_u), c_rgb_d), c_rgb); */
    vec4 c_mix = (c_rgb + c_rgb_l + c_rgb_r + c_rgb_u + c_rgb_d) * 0.2;
    vec3 c_mix_hsv  = rgb2hsv(c_mix.xyz); // .x = hue, .y = saturation, .z = value
    vec3 c_here_hsv = rgb2hsv(c_rgb.xyz); // .x = hue, .y = saturation, .z = value

    /* if (invert_color && (c_here_hsv.z < 0.01 || c_here_hsv.z > 0.99)) */
    /*     c_rgb = vec4(vec3(c_rgb.a, c_rgb.a, c_rgb.a) - vec3(c_rgb), c_rgb.a); */
    if (c_mix_hsv.z < 0.25)
        c_rgb *= opacity;
    /* else */
    /*     c_rgb *= mix(opacity, 1.0, clamp(10*abs(c_mix_hsv.z - c_here_hsv.z), 0.0, 1.0)); */

    gl_FragColor = c_rgb;
}
