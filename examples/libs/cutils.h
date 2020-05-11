#ifndef HEADERFILE_H
#define HEADERFILE_H

#define __START(op, A, B, C, D, E, F, G, H, I, J, N, ...) __ARGS_##N(op, A, B, C, D, E, F, G, H, I, J)
#define __ARGS_1(op, A, B, C, D, E, F, G, H, I, J) A
#define __ARGS_2(op, A, B, C, D, E, F, G, H, I, J) __##op(A, B)
#define __ARGS_3(op, A, B, C, D, E, F, G, H, I, J) __##op(A, __##op(B, C))
#define __ARGS_4(op, A, B, C, D, E, F, G, H, I, J) __##op(__##op(A, B), __##op(C, D))
#define __ARGS_5(op, A, B, C, D, E, F, G, H, I, J) __##op(A, __##op(__##op(B, C), __##op(D, E)))
#define __ARGS_6(op, A, B, C, D, E, F, G, H, I, J) __##op(__##op(A, B), __##op(__##op(C, D), __##op(E, F)))
#define __ARGS_7(op, A, B, C, D, E, F, G, H, I, J) __##op(A, __##op(__##op(B, C), __##op(__##op(D, E), __##op(F, G))))
#define __ARGS_8(op, A, B, C, D, E, F, G, H, I, J) __##op(__##op(A, B), __##op(__##op(C, D), __##op(__##op(E, F), __##op(G, H))))
#define __ARGS_9(op, A, B, C, D, E, F, G, H, I, J) __##op(A, __##op(__##op(B, C), __##op(__##op(D, E), __##op(__##op(F, G), __##op(H, I)))))
#define __ARGS_10(op, A, B, C, D, E, F, G, H, I, J) __##op(__##op(A, B), __##op(__##op(C, D), __##op(__##op(E, F), __##op(__##op(G, H), __##op(I, J)))))


#define __MIN(A, B) ((A) < (B) ? (A) : (B))
#define __MAX(A, B) ((A) > (B) ? (A) : (B))

/* Based on https://stackoverflow.com/a/57167166/2421116 */
#define min(...) __START(MIN, __VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
#define max(...) __START(MAX, __VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

#endif
