#ifndef UTF8CONV_H
#define UTF8CONV_H

void ucs32_to_utf8(uint32_t chr, int *sz, char *buf);

uint32_t utf8_to_ucs32(int sz, char *chr);

#endif /* UTF8CONV_H */
