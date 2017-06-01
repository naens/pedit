#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include "utf8conv.h"

void ucs32_to_utf8(uint32_t chr, int *sz, uint8_t *buf)
{
  if (chr < 0x80)
  {
    *sz = 1;
    buf[0] = chr;
    return;
  }
  uint8_t tmpbuf[6];
  memset(tmpbuf, 0, 6);
  uint32_t tmp = chr;
  uint8_t byte1 = 0x0;
  int i = 0;
  while (tmp > 0)
  {
    tmpbuf[i] = tmp & 0x3f | 0x80;
    tmp >>= 6;
    byte1 = (byte1 >> 1) | 0x80;
    i++;
  }
  *sz = i;
  int j = 0;
  int k = i - 1;
  while (k >=0)
  {
    buf[j] = tmpbuf[k];
    j++;
    k--;
  }
  buf[0] |= byte1;
}

uint32_t utf8_to_ucs32(int sz, uint8_t *chr)
{
  if (sz == 1)
  {
    return chr[0];
  }
  uint32_t m = 0x4;
  uint32_t res = 0;
  for (int i = 0; i < sz; i++)
  {
    res = ((res & ~m) << 6) | (chr[i] & 0x3f);
    m <<= 5;
  }
  return res & ~m;
}

void write_utf8(FILE *f, uint32_t chr)
{
  int sz;
  uint8_t buf[6];
  ucs32_to_utf8(chr, &sz, buf);
  fwrite(buf, sz, 1, f);

//  printf("write_utf8: chr=%x(%c)\tutf8={ ", chr, chr);
//  for (int i = 0; i < sz; i++) printf("%02x ", buf[i]);
//  printf("}\n");
}

uint32_t read_utf8(FILE *f)
{
  uint8_t byte1;
  if (fread(&byte1, 1, 1, f) == 1)
    return read_utf8_b1(f, byte1);
  else
    return -1;
}

uint32_t read_utf8_b1(FILE *f, uint8_t byte1)
{
  if (byte1 < 0x80)
    return byte1;

  uint32_t m = 0x80;
  uint32_t res = byte1 & ~m;
  m >>= 1;
  while (res & m)
  {
    uint8_t byte;
    if (fread(&byte, 1, 1, f) != 1)
      return -1;
    res = ((res & ~m) << 6) | (byte & 0x3f);
    m <<= 5;
  }
  return res;
}
