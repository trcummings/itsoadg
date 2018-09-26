#version 330 core

layout(location = 0) in vec3 VertexPosition_modelspace;

uniform mat4 MVP;

void main(void) {
	gl_Position = MVP * vec4(VertexPosition_modelspace, 1);
}
