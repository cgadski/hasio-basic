# Hasio BASIC

Casio BASIC has for many years inspired the coder-oriented minds of uninterested high school students. Citation: http://community.casiocalc.org/. Writing programs on a slow calculator in a limited language has a certain appeal that comes to life especially during the middle of the teacher's fourth description of affine transformation formulae, and it's become a bit of a niche past-time for a disparate few.

However, Casio BASIC is a terrible language, given any sort of objective and relevant comparison. It makes the implementation of any sort of non-trivial design pattern painful, lacking even the most basic constructs of code reusability and abstraction.

In the interest of nostalgia and general programmer-oriented fun, I spawned this project to develop a development kit, enabling a small but capable language to be transpiled, with minimal performance impact, to a subset of casio BASIC and subsequently tested on a host machine.

Yes, I am aware there is support to target gcc at prizm-executable bytecode (https://www.cemetech.net/forum/viewtopic.php?t=6185).
