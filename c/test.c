#include <stdio.h>
#include <string.h>
#include "parcel_css.h"

int print_error(CssError *error);

int main()
{
  char *source =
      ".foo {"
      "  color: yellow;"
      "}"
      ".bar {"
      "  color: yellow;"
      "}";

  CssError *error = NULL;
  StyleSheet *stylesheet = stylesheet_parse(source, strlen(source), &error);
  if (!stylesheet)
    return print_error(error);

  if (!stylesheet_transform(stylesheet, &error))
    return print_error(error);

  RawString result = stylesheet_to_css(stylesheet, &error);
  if (error)
    return print_error(error);

  stylesheet_free(stylesheet);
  fwrite(result.text, sizeof(char), result.len, stdout);
  printf("\n");
  raw_string_free(result);
}

int print_error(CssError *error)
{
  RawString message = error_message(error);
  printf("error: %.*s\n", (int)message.len, message.text);
  raw_string_free(message);
  error_free(error);
  return 1;
}
