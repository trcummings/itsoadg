#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec2 vertexPosition_screenSpace;
layout(location = 1) in vec2 vertexUV;

// Output data ; will be interpolated for each fragment.
out vec2 UV;

void main(){
  // eg. if dims are 800 x 600
  // [0..800][0..600] -> [-400..400][-300..300]
  // map [0..800][0..600] to [-1..1][-1..1]
  // Output position of the vertex, in clip space
  vec2 halfDims = vec2(400, 300);
	vec2 vertexPosition_homoneneousSpace = vertexPosition_screenSpace - halfDims;
	vertexPosition_homoneneousSpace /= halfDims;
	gl_Position = vec4(vertexPosition_homoneneousSpace, 0, 1);

	// UV of the vertex. No special space for this one.
	UV = vertexUV;
}
