#version 330 core

// Interpolated values from the vertex shaders
in vec2 texCoords;
in vec2 lightmapCoords;

// Ouput data
out vec3 color;

// Values that stay constant for the whole mesh.
uniform sampler2D textureSampler;
uniform sampler2D lightmapSampler;
uniform sampler2D textureMask;

void main(){
	vec3 lightTexel, texTexel;

	texTexel   = texture( textureSampler, texCoords ).rgb;
	// -2.0 is the "bias"
	lightTexel = texture( lightmapSampler, lightmapCoords, -2.0 ).rgb;
	// Output color = color of the texture at the specified UV
	color      = mix( texTexel, lightTexel, 1 );
}
