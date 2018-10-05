/**
 * From the OpenGL Programming wikibook: http://en.wikibooks.org/wiki/OpenGL_Programming
 * This file is in the public domain.
 * Contributors: Martin Kraus, Sylvain Beucler
 */

#version 330 core

layout(location = 0) in vec3 v_coord;
layout(location = 1) in vec3 v_normal;

uniform mat4 ModelMatrix, ViewMatrix, ProjMatrix;
uniform mat3 m_3x3_inv_transp;
uniform mat4 v_inv;

out vec4 frontColor; // color for front face
out vec4 backColor; // color for back face

// void main(void) {
//   mat4 MVP = ProjMatrix * ViewMatrix * ModelMatrix;
//
//   frontColor = vec4(0.702f, 0.729f, 0.655f, 1.0f);
//   backColor  = vec4(0.702f, 0.729f, 0.655f, 1.0f);
//   gl_Position = MVP * vec4(v_coord, 1);
// }
struct lightSource {
  vec4 position;
  vec4 diffuse;
  vec4 specular;
  float constantAttenuation, linearAttenuation, quadraticAttenuation;
  float spotCutoff, spotExponent;
  vec3 spotDirection;
};

lightSource light0 = lightSource(
  vec4(0.0,  3.0,  0.0, 1.0),
  vec4(1.0,  1.0,  1.0, 1.0),
  vec4(1.0,  1.0,  1.0, 1.0),
  0.0, 1.0, 0.0,
  180.0, 0.0,
  vec3(0.0, 0.0, 0.0)
);

vec4 scene_ambient = vec4(0.2, 0.2, 0.2, 1.0);

struct material {
  vec4 ambient;
  vec4 diffuse;
  vec4 specular;
  float shininess;
};

material frontMaterial = material(
  vec4(0.2, 0.2, 0.2, 1.0),
  vec4(1.0, 0.8, 0.8, 1.0),
  vec4(1.0, 1.0, 1.0, 1.0),
  5.0
);

material backMaterial = material(
  vec4(0.2, 0.2, 0.2, 1.0),
  vec4(0.0, 0.0, 1.0, 1.0),
  vec4(1.0, 1.0, 1.0, 1.0),
  5.0
);

void main(void) {
  mat4 MVP = ProjMatrix * ViewMatrix * ModelMatrix;
  vec3 normalDirection = normalize(m_3x3_inv_transp * v_normal);
  vec3 viewDirection = normalize(vec3(v_inv * vec4(0.0, 0.0, 0.0, 1.0) - ModelMatrix * vec4(v_coord, 1)));
  vec3 lightDirection;
  float attenuation;

	// directional light
  if (light0.position.w == 0.0) {
    // no attenuation
    attenuation = 1.0;
    lightDirection = normalize(vec3(light0.position));
  }
	// point or spot light (or other kind of light)
  else {
    vec3 vertexToLightSource = vec3(light0.position - ModelMatrix * vec4(v_coord, 1));
    float distance = length(vertexToLightSource);
    lightDirection = normalize(vertexToLightSource);
    attenuation = 1.0 / (light0.constantAttenuation
		   + light0.linearAttenuation * distance
		   + light0.quadraticAttenuation * distance * distance);

		// spotlight
    if (light0.spotCutoff <= 90.0) {
	  	float clampedCosine = max(0.0, dot(-lightDirection, normalize(light0.spotDirection)));
			// outside of spotlight cone
		  if (clampedCosine < cos(radians(light0.spotCutoff))) attenuation = 0.0;
	    else attenuation = attenuation * pow(clampedCosine, light0.spotExponent);
		}
  }

  // Computation of lighting for front faces
  vec3 ambientLighting = vec3(scene_ambient) * vec3(frontMaterial.ambient);

  vec3 diffuseReflection = attenuation
    * vec3(light0.diffuse) * vec3(frontMaterial.diffuse)
    * max(0.0, dot(normalDirection, lightDirection));

  vec3 specularReflection;
	 // light source on the wrong side?
  if (dot(normalDirection, lightDirection) < 0.0) {
    // no specular reflection
    specularReflection = vec3(0.0, 0.0, 0.0);
  }
	// light source on the right side
  else {
    specularReflection = attenuation
			* vec3(light0.specular)
			* vec3(frontMaterial.specular)
			* pow(
					max(0.0, dot(reflect(-lightDirection, normalDirection), viewDirection))
				, frontMaterial.shininess
				);
  }

  frontColor = vec4(ambientLighting + diffuseReflection + specularReflection, 1.0);

  // Computation of lighting for back faces (uses negative normalDirection and back material colors)

  vec3 backAmbientLighting = vec3(scene_ambient) * vec3(backMaterial.ambient);

  vec3 backDiffuseReflection = attenuation
    * vec3(light0.diffuse) * vec3(backMaterial.diffuse)
    * max(0.0, dot(-normalDirection, lightDirection));

  vec3 backSpecularReflection;
	// light source on the wrong side?
  if (dot(-normalDirection, lightDirection) < 0.0) {
    backSpecularReflection = vec3(0.0, 0.0, 0.0); // no specular reflection
  }
	// light source on the right side
  else {
    backSpecularReflection = attenuation
			* vec3(light0.specular)
			* vec3(backMaterial.specular)
			* pow(
					max(0.0, dot(reflect(-lightDirection, -normalDirection), viewDirection))
				, backMaterial.shininess
				);
  }

  backColor = vec4(backAmbientLighting + backDiffuseReflection + backSpecularReflection, 1.0);

  gl_Position = MVP * vec4(v_coord, 1);
}
