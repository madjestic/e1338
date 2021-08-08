#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D lower_ext;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

void main()
{
// fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
	vec4 font_clr   = texture(lower_ext,   vec2(uv.x, 1.0-uv.y));
	//fragColor = vec4( 1.0f, 1.0f, 1.0f, 1.0f);
	fragColor = vec4(vec3(font_clr.r), font_clr.r*2.0);
	// TODO : figure out what's up with uv.y
	// fragColor = vec4( 0, uv.y*1, 0.0, 1.0);
}
