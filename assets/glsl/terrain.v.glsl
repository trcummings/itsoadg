#version 330 core

layout(location = 0) in vec3 position;

uniform mat4 transformationMatrix;
uniform mat4 projectionMatrix;
uniform mat4 viewMatrix;

void main(void){
	vec4 modelMatrix = transformationMatrix * vec4(position, 1);
	gl_Position = projectionMatrix * viewMatrix * modelMatrix;
}
