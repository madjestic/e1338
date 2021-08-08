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
// fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
	vec4 font_clr   = texture(checkerboard,   vec2(uv.x, 1.0-uv.y));
	//fragColor = vec4( 1.0f, 1.0f, 1.0f, 1.0f);
	fragColor = font_clr;
	// TODO : figure out what's up with uv.y
	// fragColor = vec4( 0, uv.y*1, 0.0, 1.0);
}
