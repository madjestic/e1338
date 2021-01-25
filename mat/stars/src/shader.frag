#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

float fit (in float val, in float smin, in float smax, in float dmin, in float dmax)
{
	return (dmax - dmin)*((val - smin)/(smax - smin)) + dmin;
}

vec3 fit3 (in vec3 val, in float smin, in float smax, in float dmin, in float dmax)
{
	return (dmax - dmin)*((val - smin)/(smax - smin)) + dmin;
}

vec3 avg3 (in vec3 val)
{
	return (vec3(val.r + val.g + val.z)/3.0f);
}

vec3 blend3 (in vec3 a, in vec3 b, in float s)
{
	return (a*(1.0f - s) + b*s);
}	

void main()
{
	vec3 tmp3 = vec3 (Cd.x, Cd.y, Cd.z);
	tmp3 = fit3(tmp3, 0.85, 1.0, 0.6, 1.0);
	tmp3 = blend3(avg3(tmp3), tmp3, 0.3);
	//fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
	fragColor = vec4( tmp3, A );
}
