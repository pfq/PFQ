-- example
--

filterFunction = par ip ip6

main =
    steer_flow >->
    filterFunction >-> when is_tcp drop >-> kernel

