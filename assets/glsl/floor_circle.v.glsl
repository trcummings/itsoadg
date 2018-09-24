#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 VertexPosition_WorldSpace;

// Output data ; will be interpolated for each fragment.
// out vec2 UV;

// Values that stay constant for the whole mesh.
uniform mat4 ModelMatrix;
uniform mat4 ProjMatrix;
uniform mat4 ViewMatrix;

void main(void){
	// Output position of the vertex
	gl_Position = ProjMatrix * ViewMatrix * ModelMatrix * vec4(VertexPosition_WorldSpace, 1.0f);
}
