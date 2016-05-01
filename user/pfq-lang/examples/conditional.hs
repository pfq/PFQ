-- example
--

import Network.PFQ.Lang.Prelude

cond_filter = par (when (not is_tcp) drop) (filter (ip_ttl .> 10))

main = if (not is_tcp)
        then cond_filter
        else filter (ip_ttl .> 10)

