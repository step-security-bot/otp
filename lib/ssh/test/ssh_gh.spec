{alias,dir,"../ssh_test"}.
{suites,dir,all}.

{skip_suites, dir, [ssh_bench_SUITE,
                    ssh_upgrade_SUITE
                   ],
 "Benchmarks run separately"}.
{skip_cases, dir, ssh_options_SUITE, [max_log_item_len], "Unstable testcases"}.
