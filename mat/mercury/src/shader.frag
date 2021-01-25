#version 430

uniform float     u_time;
uniform vec2      u_resolution;
uniform sampler2D mercury_1024_512;

uniform vec3      sunP;

in vec4 gl_FragCoord;
in float A;
in vec3  N;
in vec3  Ng;
in vec3  Cd;
in vec3  uv;
in vec3  P;

out vec4 fragColor;

void main()
{
	vec3 normal =
		vec3( N.x*0.5+0.5
			, N.y*0.5+0.5
			,-N.z*0.5+0.5 );
	
	//vec3 sunP          = vec3 (299999999999.0, .0, .0);
	vec4 day_map_clr   = texture(mercury_1024_512,   vec2(uv.x, uv.y));
	vec4 night_map_clr = vec4 (.0, .0, .0, 1.0);
	vec4 cloud_map_clr = vec4 (.0, .0, .0, 1.0);
	vec4 tint          = vec4 (1., .9804, .642, 1.);

	vec3 dir               = sunP - P;
	float dot_product_mask = dot(Ng, normalize(dir));
	float s                = 1.5f;
	vec4 night_map_contr   = clamp((-dot_product_mask) * pow(clamp((night_map_clr), 0.0f, 1.0f) * 3.0f, vec4(s,s,s,s)), 0.0f, 1.0f);
	
	float day_mask         = clamp((dot_product_mask * .5 + .0) * 1.0f, 0.0f, 1.0f);
	float noise            = 1.0f; // a good place to add a noisy cloud edge
	float penumbra_mask    = noise
		                   * clamp (dot_product_mask * .5 + 0.175f, 0.0f, 1.0f);

	float light_mask       = day_mask + penumbra_mask;

	vec4 cloud_mask        = cloud_map_clr * clamp(light_mask, .0, 1.);
	
	vec4 cloud_shadows     = clamp(((1.0f - cloud_map_clr*0.9f)*1.2f), .0, 1.);
	cloud_shadows.a        = 1.;

	s = light_mask;
	vec4 light_mask4       = vec4(s,s,s,1.0);
	vec4 day_map_contr     = clamp((light_mask4) * day_map_clr, .0, 1.);

	vec4 clr               = tint
		                   * (day_map_contr + night_map_contr)
		                   * cloud_shadows
		                   + cloud_mask;

	fragColor = clr;
}
