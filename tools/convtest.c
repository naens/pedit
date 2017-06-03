#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "utf8conv.h"

int main (int argc, char **argv)
{
  if (argc == 2) /* argv[1] is ucs32 */
  {
    uint32_t chr = strtol(argv[1], NULL, 16);
    uint8_t buf[6];
    int sz;
    ucs32_to_utf8(chr, &sz, buf);
    for (int i = 0; i < sz; i++)
      printf("%02x ", buf[i]);
    printf("\n");
  }
  else /* argv contains utf8 chars */
  {
    int sz = argc - 1;
    uint8_t buf[sz];
    for (int i = 0; i < sz; i++)
      buf[i] = strtol(argv[i + 1], NULL, 16);
    printf("%x\n", utf8_to_ucs32(sz, buf));
  }
}
