-- regression test...


predicates =
    is_ip                               .||.
    is_udp                              .||.
    is_tcp                              .||.
    is_icmp                             .||.
    is_ip6                              .||.
    is_udp6                             .||.
    is_tcp6                             .||.
    is_icmp6                            .||.
    is_flow                             .||.
    is_l3_proto 42                      .||.
    is_l4_proto 8                       .||.
    is_frag                             .||.
    is_first_frag                       .||.
    is_more_frag                        .||.
    is_rtp                              .||.
    is_rtcp                             .||.
    is_sip                              .||.
    is_voip                             .||.
    has_port 1024                       .||.
    has_src_port 21                     .||.
    has_dst_port 80                     .||.
    has_addr "192.168.0.0/24"           .||.
    has_addr (CIDR ("192.168.0.0", 24)) .||.
    has_dst_addr "10.0.0.0/16"          .||.
    has_state 45                        .||.
    has_mark 11                         .||.
    has_vlan                            .||.
    has_vid 42                          .||.
    vlan_id [1, 12]


combinators = is_ip .||. is_ip6 .&&. (not is_tcp .^^. is_udp)


combinators2 = par ip (inv udp)


cond = do
    conditional is_tcp (log_msg "tcp") unit
    when is_tcp (drop)
    unless is_ip6 kernel


filters = do
    ip
    ip6
    udp
    tcp
    icmp
    udp6
    tcp6
    icmp6
    vlan
    l3_proto 42
    l3_proto 0x842
    l4_proto 89
    flow
    rtp
    rtcp
    sip
    voip
    vlan_id_filter [1,2,3]
    no_frag
    no_more_frag
    port 12
    src_port 21
    dst_port 22
    addr "192.168.0.0/24"
    addr (CIDR ("192.168.0.0",24))
    src_addr "0.0.0.0/0"
    dst_addr "0.0.0.0/0"


steerings = do
    steer_rrobin
    steer_rss
    steer_to 1
    steer_link
    steer_link_local "4c:60:de:86:55:46"
    steer_mac
    steer_vlan
    steer_p2p
    steer_ip
    -- steer_ip_local "192.168.1.0/24"
    steer_ip6
    steer_p2p6
    steer_flow
    steer_net "192.168.0.0" 16 24
    steer_field 14 2
    steer_field_double 14 18 2
    steer_field_symmetric 14 18 4
    steer_rtp
    steer_voip


forwarders = do
    kernel
    broadcast
    drop
    classify 11
    forward "eth1"
    forwardIO "eth2"
    link ["eth1", "eth2", "eth3"]
    bridge "eth2"
    tee "eth1" is_udp >-> kernel
    tap "eth1" is_udp >-> kernel


logging = do
    log_msg "ok"
    log_buff
    log_packet



blooming = do
    filter (bloom 1024 ["192.168.0.13", "192.168.0.42"] 32)
    filter (bloom_src 1024 ["192.168.0.13", "192.168.0.42"] 32)
    filter (bloom_dst 1024 ["192.168.0.13", "192.168.0.42"] 32)
    bloom_filter 1024 ["192.168.0.13", "192.168.0.42"] 32
    bloom_src_filter 1024 ["192.168.0.13", "192.168.0.42"] 32
    bloom_dst_filter 1024 ["192.168.0.13", "192.168.0.42"] 32


misc = do
    unit
    inc 10
    dec 11
    mark 42
    put_state 42


main = do
    filter predicates
    filter combinators
    combinators2
    cond
    filters
    steerings
    forwarders
    logging
    blooming
    misc

