#version 330 core

layout(location = 0) in vec3 VertexPosition_modelspace;

out vec3 BoxFragColor;

uniform vec3 BoxColor;
uniform mat4 ModelMatrix;
uniform mat4 ViewMatrix;
uniform mat4 ProjMatrix;

void main(void) {
	gl_Position = ProjMatrix * ViewMatrix * ModelMatrix * vec4(VertexPosition_modelspace, 1);

	BoxFragColor = BoxColor;
}
