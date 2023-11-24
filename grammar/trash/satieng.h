/* A packrat parser generated by PackCC 1.8.0 */

#ifndef PCC_INCLUDED_SATIENG_H
#define PCC_INCLUDED_SATIENG_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct satie_context_tag satie_context_t;

satie_context_t *satie_create(void *auxil);
int satie_parse(satie_context_t *ctx, int *ret);
void satie_destroy(satie_context_t *ctx);

#ifdef __cplusplus
}
#endif

#endif /* !PCC_INCLUDED_SATIENG_H */
