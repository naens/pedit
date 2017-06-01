#ifndef UTF8CONV_H
#define UTF8CONV_H

void ucs32_to_utf8(uint32_t chr, int *sz, uint8_t *buf);

uint32_t utf8_to_ucs32(int sz, uint8_t *chr);


void write_utf8(FILE *f, uint32_t chr);

uint32_t read_utf8(FILE *f);

uint32_t read_utf8_b1(FILE *f, uint8_t byte1);

#endif /* UTF8CONV_H */
