#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct CssError CssError;

typedef struct StyleSheet StyleSheet;

typedef struct Targets {
  uint32_t android;
  uint32_t chrome;
  uint32_t edge;
  uint32_t firefox;
  uint32_t ie;
  uint32_t ios_saf;
  uint32_t opera;
  uint32_t safari;
  uint32_t samsung;
} Targets;

typedef struct ParseOptions {
  const char *filename;
  bool nesting;
  bool custom_media;
  bool css_modules;
  const char *css_modules_pattern;
  bool css_modules_dashed_idents;
  bool error_recovery;
} ParseOptions;

typedef struct TransformOptions {
  struct Targets targets;
  char **unused_symbols;
  uintptr_t unused_symbols_len;
} TransformOptions;

typedef struct RawString {
  char *text;
  uintptr_t len;
} RawString;

typedef struct ToCssResult {
  struct RawString code;
  struct RawString map;
} ToCssResult;

typedef struct PseudoClasses {
  const char *hover;
  const char *active;
  const char *focus;
  const char *focus_visible;
  const char *focus_within;
} PseudoClasses;

typedef struct ToCssOptions {
  bool minify;
  bool source_map;
  const char *input_source_map;
  uintptr_t input_source_map_len;
  struct Targets targets;
  bool analyze_dependencies;
  struct PseudoClasses pseudo_classes;
} ToCssOptions;

bool browserslist_to_targets(const char *query, struct Targets *targets, struct CssError **error);

struct StyleSheet *stylesheet_parse(const char *source,
                                    uintptr_t len,
                                    struct ParseOptions options,
                                    struct CssError **error);

bool stylesheet_transform(struct StyleSheet *stylesheet,
                          struct TransformOptions options,
                          struct CssError **error);

struct ToCssResult stylesheet_to_css(struct StyleSheet *stylesheet,
                                     struct ToCssOptions options,
                                     struct CssError **error);

void stylesheet_free(struct StyleSheet *stylesheet);

void to_css_result_free(struct ToCssResult result);

const char *error_message(struct CssError *error);

void error_free(struct CssError *error);
