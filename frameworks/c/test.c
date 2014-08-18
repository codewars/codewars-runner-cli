#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define MAX_CHARS 1024

char * format(const char * format_string, ...) {
  //! Create a new string, up to MAX_CHARS long
  //! Uses vsnprintf to format it, and returns it
  //! Users are responsible for freeing outputted string
  char * output_string = malloc(MAX_CHARS);
  va_list arguments;
  va_start(arguments, format_string);
  vsnprintf(output_string, MAX_CHARS, format_string, arguments);
  va_end(arguments);
  return output_string;
}

char * __CODEWARS__default_string__
(char * default_string, ...) {
  //! Defaults to a particular value
  //! when no optional second argument is passed
  //! arguments must be terminted with NULL
  //! NOTE: this is intended for use in macros only!
  va_list arguments;
  char * second_argument;
  va_start(arguments, default_string);
  second_argument = va_arg(arguments, char*);
  va_end(arguments);
  return second_argument ? second_argument : default_string;
}

int __CODEWARS__stdout__(const char * first, ...) {
  //! Writes several strings to stdout
  //! Arguments must be terminated with NULL
  //! Returns the number of strings written
  char * arg;
  int i = 0;
  va_list arguments;
  fputs(first, stdout);
  va_start(arguments, first);
  while((arg = va_arg(arguments, char*))) {
    fputs(arg, stdout);
    ++i;
  }
  va_end(arguments);
  return i;
}

void __CODEWARS__fail__(const char * message) {
  //! Prints an error message and exits with failure status
  __CODEWARS__stdout__("<FAILED::>", message, "\n", NULL);
  exit(EXIT_FAILURE);
}
