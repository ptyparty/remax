/* Pointer-based wrappers around the tree-sitter calls that pass TSNode
 * structs by value, which Haskell's ccall FFI cannot express directly. */

#include <stdlib.h>
#include "tree_sitter/api.h"

TSNode *remax_tree_root_node(TSTree *tree) {
  TSNode *node = malloc(sizeof(TSNode));
  if (node != NULL) {
    *node = ts_tree_root_node(tree);
  }
  return node;
}

void remax_node_free(TSNode *node) {
  free(node);
}

void remax_query_cursor_exec(TSQueryCursor *cursor, const TSQuery *query,
                             TSNode *node) {
  ts_query_cursor_exec(cursor, query, *node);
}

/* Step to the next whole match, copying out its pattern index and up to
 * `max` captures as (start byte, end byte, capture id) triples; returns 1,
 * or 0 when the matches are exhausted. */
int remax_query_cursor_next_match(TSQueryCursor *cursor,
                                  uint32_t *pattern_index, uint32_t *starts,
                                  uint32_t *ends, uint32_t *ids, uint32_t max,
                                  uint32_t *count) {
  TSQueryMatch match;
  if (!ts_query_cursor_next_match(cursor, &match)) {
    return 0;
  }
  *pattern_index = match.pattern_index;
  uint32_t n = match.capture_count < max ? match.capture_count : max;
  for (uint32_t i = 0; i < n; i++) {
    starts[i] = ts_node_start_byte(match.captures[i].node);
    ends[i] = ts_node_end_byte(match.captures[i].node);
    ids[i] = match.captures[i].index;
  }
  *count = n;
  return 1;
}
