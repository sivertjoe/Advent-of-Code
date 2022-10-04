#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <stdint.h>


int NUMS[200];
#define ROW 6 // 4 numbers + 1 \n + 1 \0

int read_nums(char* filename)
{
  FILE* file = fopen(filename, "r");
  if(file == NULL)
  {
    printf("Error\n");
    exit(-1);
  }
  
  char buffer[ROW];
  
  int idx = 0;
  while(fgets(buffer, ROW, file)) 
  {
    NUMS[idx++] = atoi(buffer);
  }
  
  fclose(file);
  return idx;
}

int part_one(int max)
{
  for(int i = 0; i < max; i++)
  {
    for(int j = i + 1; j < max; j++)
    {
      if(NUMS[i] + NUMS[j] == 2020)
      {

        return NUMS[i] * NUMS[j];
      }
    }
  }
  return -1;
}

int part_two(int max)
{
  for(int i = 0; i < max; i++)
  {
    for(int j = i + 1; j < max; j++)
    {
      for(int k = j + 1; k < max; k++)
      {
        if(NUMS[i] + NUMS[j] + NUMS[k] == 2020)
        {
          return NUMS[i] * NUMS[j] * NUMS[k];
        }
        
      }
    }
  }
  return -1;
}

void time_func(int (*f)(int), int arg)
{
    struct timespec start, end;

    clock_gettime(CLOCK_MONOTONIC_RAW, &start);
    int ans = f(arg);
    clock_gettime(CLOCK_MONOTONIC_RAW, &end);

    long time_spent = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_nsec - start.tv_nsec) / 1000;
    time_spent /= 1000;

    printf("(%lums)\t%d\n", time_spent, ans);
}
 //(0ms)	3488702

int main(int argc, char** argv)
{
  if(argc != 2)
  {
    printf("Specify file\n");
    exit(-1);
  }

  int max = read_nums(argv[1]);
  time_func(part_one, max);
  time_func(part_two, max);
}

