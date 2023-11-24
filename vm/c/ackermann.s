import std.concurrency
import std.stdio

main() {
  jids = Ackermann.startJobs(3, 10)
  Ackermann.waitForJobs(jids)
}

singleton Ackermann {
    public startJobs(m, n, i = 0, jids = []) {
        if i < n {
            computeAckermann(fromJid, m, n) {
                result = ackermann(m, n)
                send fromJid #(self, m, n, result)
            }
            jid = mspawn computeAckermann(self, m, ++i)
            concurrency.setMaxMailboxSize(jid, 4, concurrency.OnCrowding.block)
            startJobs(m, n, i, jids ~ jid)
        }
        jids
    }

    public waitForJobs(jids) {
        if jids.length > 0 {
            receive {
                #(?jid, ?m, ?n, ?result) {
                    stdio.writeln("ackermann($m, $n) = $result")
                }
                #(JobMonitor.died, ?jid, ?reason) {
                    stdio.writeln("Oh no! Compute job $jid died: $reason")
                }
            }
            waitForJobs(jids[0 .. $ - 1])
        }
    }

    private ackermann(m, n) {
        if m == 0 {
            n + 1
        } else if n == 0 {
            ackermann(m - 1, 1)
        } else {
            ackermann(m - 1, ackermann(m, n - 1))
        }
    }
}
