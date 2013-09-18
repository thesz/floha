floha
=====

Data flow hardware description language

The name Floha stands for "data FLOw HArdware", or "data FLOw HAskell", whatever suits you better. There's also
a river Floha (o-umlaut) in Germany, so one more association with "flow of something".

It is similar to CAPH data flow hardware language: http://wwwlasmea.univ-bpclermont.fr/Personnel/Jocelyn.Serot/caph.html

I hope to fix some deficiencies in CAPH. Namely:
 * CAPH does not support multiclock networks, it also does not provide means to separate or speed up networks with different types of FIFOs.
 * CAPH does not properly support memory blocks in FPGA. Most such blocks returns results in the next clock cycle and
   that should be accounted in the compilation process.
 * CAPH does not support complex types, even tuples, nevermind algebraic types.

Content of the package
======================

The src/ directory contains source code of Floha. It is licensed under BSD license because it is essentially a language.

The examples/ directory provide examples of various circuits.
