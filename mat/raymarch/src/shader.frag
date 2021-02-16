#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D tex_00;

uniform mat4 camera;
uniform mat4 persp;
uniform mat4 xform;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;

out vec4 fragColor;

// params:
// p: arbitrary point in 3D space
// c: the center of our sphere
// r: the radius of our sphere
float distance_from_sphere(in vec3 p, in vec3 c, float r)
{
  return length(p - c) - r;
}

float map_the_world (in vec3 p)
{
  float displacement = sin(5.0 * p.x) * sin(5.0 * p.y) * sin(5.0 * p.z) * 0.25;
  float sphere_0 = distance_from_sphere (p, vec3(0.0), 1.0);

  return sphere_0 + displacement;
}

vec3 calculate_normal(in vec3 p)
{
    const vec3 small_step = vec3(0.001, 0.0, 0.0);

    float gradient_x = map_the_world(p + small_step.xyy) - map_the_world(p - small_step.xyy);
    float gradient_y = map_the_world(p + small_step.yxy) - map_the_world(p - small_step.yxy);
    float gradient_z = map_the_world(p + small_step.yyx) - map_the_world(p - small_step.yyx);

    vec3 normal = vec3(gradient_x, gradient_y, gradient_z);

    return normalize(normal);
}

vec3 ray_march(in vec3 ro, in vec3 rd)
{
  // Assume that `ro` and `rd` are defined elsewhere
  // and represent the ray's origin and direction, 
  // respectively
  
  float total_distance_traveled = 0.0;
  const int NUMBER_OF_STEPS = 32;
  const float MINIMUM_HIT_DISTANCE = 0.001;
  const float MAXIMUM_TRACE_DISTANCE = 1000.0;  

  for (int i = 0; i < NUMBER_OF_STEPS; ++i)
    {
      // Calculate our current position along the ray
      vec3 current_position = ro + total_distance_traveled * rd;

      // Some code to evaluate our SDF and determine whether or not 
      // we've hit a surface based on our current position...
      float distance_to_closest = map_the_world (current_position);

      if (distance_to_closest < MINIMUM_HIT_DISTANCE) // hit
        {
	  vec3 normal = calculate_normal(current_position);

	  vec3 light_position = vec3(2.0, -5.0, 3.0);

	  // Calculate the unit direction vector that points from
	  // the point of intersection to the light source
	  vec3 direction_to_light = normalize(current_position - light_position);

	  float diffuse_intensity = max(0.0, dot(normal, direction_to_light));

	  // We hit something! Return red for now
	  //return vec3(1.0, 0.0, 0.0);
	  //return normal * 0.5 + 0.5;
	  return vec3(1.0, 0.0, 0.0) * diffuse_intensity;
        }

      if (total_distance_traveled > MAXIMUM_TRACE_DISTANCE) // miss
        {
	  break;
        }

      // accumulate the distance traveled thus far
      total_distance_traveled += distance_to_closest;
    }

  // If we get here, we didn't hit anything so just
  // return a background color (black)
  return vec3(0.0);
}

void main()
{

  mat3 viewRot =
    mat3(-camera[0].xyz
	,-camera[1].xyz
	, camera[2].xyz );

  mat4 cameraM =
    mat4 ( camera[0]
	 , camera[1]
	 , camera[2]
	 , vec4(0,0,xform[3].z,1));
  
  vec2 uv  = uv.st * 2.0 - 1.0;
  
  float aspr = u_resolution.x / u_resolution.y; //aspect ratio
  uv.x *= aspr;

  //vec3 camera_position = vec3(0.0, 0.0, -5.0);
  vec3 camera_position = xform[3].xyz;
  vec3 ro = camera_position;
  vec3 rd = vec3 (uv, 1.0);

  vec3 shaded_color = ray_march(viewRot * ro, viewRot * rd);

  //fragColor = vec4( Cd.x, Cd.y, Cd.z, A );
  //fragColor = vec4( 0.0, Cd.y, Cd.z, A );
  fragColor = vec4( shaded_color, 1.0 );
}
