#include <stdio.h>

__global__ void helloworld() {
  printf("Hello, World!\n");
}

int main() {
  helloworld<<<1,1>>>();
  cudaDeviceSynchronize();
  return 0;
}

