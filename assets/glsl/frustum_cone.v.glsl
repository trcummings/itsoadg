#version 330 core

layout(location = 0) in vec3 VertexPosition_modelspace;

uniform mat4 ModelMatrix;
uniform mat4 ProjMatrix;
uniform mat4 ViewMatrix;

void main(void) {
	gl_Position = ProjMatrix * ViewMatrix * ModelMatrix * vec4(VertexPosition_modelspace, 1);
}
