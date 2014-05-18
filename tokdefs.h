#ifndef BISON_Y_TAB_H
# define BISON_Y_TAB_H

# ifndef YYSTYPE
#  define YYSTYPE int
#  define YYSTYPE_IS_TRIVIAL 1
# endif
# define	tok_identifier	257
# define	tok_array_identifier	258
# define	tok_label	259
# define	tok_integer_const	260
# define	tok_real_const	261
# define	tok_dp_const	262
# define	tok_quad_const	263
# define	tok_complex_const	264
# define	tok_dcomplex_const	265
# define	tok_logical_const	266
# define	tok_string	267
# define	tok_hollerith	268
# define	tok_edit_descriptor	269
# define	tok_letter	270
# define	tok_relop	271
# define	tok_AND	272
# define	tok_OR	273
# define	tok_EQV	274
# define	tok_NEQV	275
# define	tok_NOT	276
# define	tok_power	277
# define	tok_concat	278
# define	tok_lparen	279
# define	tok_ACCEPT	280
# define	tok_ASSIGN	281
# define	tok_BACKSPACE	282
# define	tok_BLOCKDATA	283
# define	tok_BYTE	284
# define	tok_CALL	285
# define	tok_CASE	286
# define	tok_CASEDEFAULT	287
# define	tok_CHARACTER	288
# define	tok_CLOSE	289
# define	tok_COMMON	290
# define	tok_COMPLEX	291
# define	tok_CONTINUE	292
# define	tok_CYCLE	293
# define	tok_DATA	294
# define	tok_DIMENSION	295
# define	tok_DO	296
# define	tok_DOUBLECOMPLEX	297
# define	tok_DOUBLEPRECISION	298
# define	tok_DOWHILE	299
# define	tok_ELSE	300
# define	tok_END	301
# define	tok_ENDBLOCKDATA	302
# define	tok_ENDDO	303
# define	tok_ENDFILE	304
# define	tok_ENDFUNCTION	305
# define	tok_ENDIF	306
# define	tok_ENDPROGRAM	307
# define	tok_ENDSELECT	308
# define	tok_ENDSUBROUTINE	309
# define	tok_ENTRY	310
# define	tok_EQUIVALENCE	311
# define	tok_EXTERNAL	312
# define	tok_EXIT	313
# define	tok_FORMAT	314
# define	tok_FUNCTION	315
# define	tok_GOTO	316
# define	tok_IF	317
# define	tok_IMPLICIT	318
# define	tok_INCLUDE	319
# define	tok_INQUIRE	320
# define	tok_INTEGER	321
# define	tok_INTRINSIC	322
# define	tok_LOGICAL	323
# define	tok_NAMELIST	324
# define	tok_NONE	325
# define	tok_OPEN	326
# define	tok_PARAMETER	327
# define	tok_PAUSE	328
# define	tok_POINTER	329
# define	tok_PRINT	330
# define	tok_PROGRAM	331
# define	tok_READ	332
# define	tok_REAL	333
# define	tok_RETURN	334
# define	tok_REWIND	335
# define	tok_SAVE	336
# define	tok_SELECTCASE	337
# define	tok_STOP	338
# define	tok_SUBROUTINE	339
# define	tok_THEN	340
# define	tok_TO	341
# define	tok_TYPE	342
# define	tok_WHILE	343
# define	tok_WRITE	344
# define	tok_illegal	345
# define	tok_empty	346
# define	EOS	127
# define	REDUCE	347


extern YYSTYPE yylval;

#endif /* not BISON_Y_TAB_H */
