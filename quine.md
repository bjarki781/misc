* quine in c
- the parts referencing themselves were the worst (duh)
- quine.c contains the original program
- o1.c contains the actual fixed point
- basically works through the idea of printf("%s", "%s") with some noise
- the main problem turns out to be quoting, the act of printing something
  lowers the quoting level so we have to increase it when printing the string
  that contains program