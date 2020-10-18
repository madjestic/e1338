#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D intro_e1338;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 outCd;

void main()
{
	vec4 texCd = vec4(texture(intro_e1338, vec2(uv.x, uv.y)).rgb, 1.0);
	vec4 vtxCd = vec4( Cd.x, Cd.y, Cd.z, A );
	outCd = texCd;
}
