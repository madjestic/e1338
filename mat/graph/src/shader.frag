#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D checkerboard;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
	vec4 texCd = vec4(texture(checkerboard, vec2(uv.x, uv.y)).rgb, 1.0);
	fragColor = texCd;
}
