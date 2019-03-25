# Depends on purrr, tibble and dplyr
get_all_cycles = function(edge_list, silent=FALSE) {
  stopifnot(length(colnames(edge_list)) == 2)
  
  id_map = c(unique(edge_list[[1]]), unique(edge_list[[2]])) %>%
    unique() %>%
    set_names(seq_along(.), .)
  
  id_edges = edge_list %>%
    set_names(c('from', 'to')) %>%
    mutate_all(funs(unname(id_map[.])))
  
  cycle_pack = rlang::env(
    cycles = tibble(name=integer(0)),
    current_paths = rename(id_edges, start=from)
  )
  
  error_limit = 20
  current_val = 1
  while(nrow(cycle_pack$current_paths) > 0 & current_val < error_limit) {
    if(!silent) cat('Cycle join number', current_val, '\n')
    join_result = cycle_pack$current_paths %>%
      anti_join(cycle_pack$cycles, c(to='name')) %>%
      inner_join(id_edges, c(to='from')) %>%
      distinct(start, to = to.y)
    cycle_filter = with(join_result, start == to)
    cycle_pack$cycles = join_result %>%
      filter(cycle_filter) %>%
      select(name=start) %>%
      bind_rows(cycle_pack$cycles)
    cycle_pack$current_paths = join_result %>% filter(!cycle_filter)
    current_val = current_val + 1
  }
  
  path_accumulator = cycle_pack$cycles %>%
    distinct() %>%
    transmute(start = name, to = name, path = as.list(to), cycle_found = FALSE)
  while(any(!is.na(path_accumulator$to))) {
    path_accumulator = path_accumulator %>%
      left_join(id_edges, c(to = 'from')) %>%
      transmute(start, to = to.y, path = map2(path, to, ~ c(.x, na.omit(.y))), cycle_found) %>%
      filter(!is.na(to) | cycle_found) %>%
      mutate(cycle_found = map_lgl(path, ~ anyDuplicated(.x)>0), to = ifelse(cycle_found, NA, to))
  }
  
  path_accumulator %>%
    mutate(cycle_length =map_int(path, length), cycle_start = cycle_length - map_int(path, . %>% rev %>% anyDuplicated)) %>%
    mutate(cycle_nodes = pmap(list(path, cycle_start, cycle_length - 1), ~ ..1[..2:..3])) %>%
    mutate(path_summary = map_chr(cycle_nodes, . %>% sort %>% paste0(collapse='>'))) %>%
    distinct(path_summary, .keep_all=TRUE) %>%
    mutate(cycle_length = map_int(cycle_nodes, length), cycle_nodes) %>%
    arrange(cycle_length, start, map_int(cycle_nodes, first)) %>%
    transmute(cycle_id = row_number(), cycle_length, cycle_nodes) %>%
    unnest() %>%
    mutate(cycle_nodes = names(id_map)[cycle_nodes])
}
