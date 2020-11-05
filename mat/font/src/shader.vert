#version 430 core

layout(location = 0) in float alpha;
layout(location = 1) in vec3 color;
layout(location = 2) in vec3 normal;
layout(location = 3) in vec3 uvCoords;
layout(location = 4) in vec3 vPosition;

uniform mat4  camera;
uniform mat4  persp;
uniform mat4  xform;
uniform mat4  xform1;

// Output data ; will be interpolated for each fragment.
out float A;
out vec3  N;
out vec3  Ng;
out vec3  Cd;
out vec3  uv;

float log10(in float x)
{
	float far  = 10.0;
	float result = (1.0f / log(far)) * log(x);
	return(result);
}

void main()
{
	mat3 viewRot =
		mat3( camera[0].xyz
			, camera[1].xyz
			, camera[2].xyz );
	
	mat4 cameraRot =
		mat4 ( camera[0]
			 , camera[1]
			 , camera[2]
			 , vec4(0,0,0,1));

	mat3 perspRot =
		mat3 ( persp[0].xyz
			 , persp[1].xyz
			 , persp[2].xyz );

	mat4 xform    = xform1;
	mat3 xformRot =
		mat3 ( xform[0].xyz
			 , xform[1].xyz
			 , xform[2].xyz );	
	
	A  = alpha;
	N  = normalize(perspRot * viewRot * xformRot * normal);
	Ng = normalize(normal);
	Cd = color;
	uv = uvCoords;

	vec4 position = vec4(vPosition,1.0);	

	gl_Position
		= persp
		//* cameraRot
		* xform
		* position;

	gl_Position.z = -log10(length(gl_Position.xyz));
} 
