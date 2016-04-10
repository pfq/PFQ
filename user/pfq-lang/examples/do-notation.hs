-- example
--

import Lib.Filter

main = do
    trace
    shift (do
            trace
            shift (do
                trace
                log_msg "hello"
                )
           )
    trace


