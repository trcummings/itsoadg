#version 330 core

// Interpolated values from the vertex shaders
in vec2 UV;

// Ouput data
out vec4 color;

uniform sampler2D TextureSampler;

void main(void) {
	// Output color = color of the texture at the specified UV
	color = texture(TextureSampler, UV);
}
