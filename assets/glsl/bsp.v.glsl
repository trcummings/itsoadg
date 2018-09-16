#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 vertexPosition;
layout(location = 1) in vec2 textureUV;
layout(location = 2) in vec2 lightmapUV;

// Output data ; will be interpolated for each fragment.
out vec2 texCoords;
out vec2 lightmapCoords;

// Values that stay constant for the whole mesh.
uniform mat4 view;
uniform mat4 proj;

void main(){

	// Output position of the vertex, in clip space : VP * position
	gl_Position = proj * view * vec4(vertexPosition, 1.0f);

	// UV of the vertex. No special space for this one.
	texCoords      = textureUV;
  lightmapCoords = lightmapUV;
}
