#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec4 vertexNormal_modelspace;

// Output data ; will be interpolated for each fragment.
out vec3 Normal;
out vec3 LightRay_reverse;
out vec3 EyeDirection;
out float Normal_sign; // 0 - negative, 1 - positive

// Values that stay constant for the whole mesh.
uniform mat4 MVP;
uniform vec3 LightPos;
uniform vec3 EyePos;

void main(){

	// Output position of the vertex, in clip space : MVP * position
	gl_Position =  MVP * vec4(vertexPosition_modelspace,1);

	LightRay_reverse = LightPos - vertexPosition_modelspace;
	EyeDirection = EyePos - vertexPosition_modelspace;
	Normal_sign = vertexNormal_modelspace.w / 255.0;
	float w = (Normal_sign*2.0-1.0);

	Normal.x = ((vertexNormal_modelspace.x / 255.0)*2.0-1.0)*w;
	Normal.y = ((vertexNormal_modelspace.y / 255.0)*2.0-1.0)*w;
	Normal.z = ((vertexNormal_modelspace.z / 255.0)*2.0-1.0)*w;
}