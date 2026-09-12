{% if refused %}`webfetch` will not reach `{{host}}`.

That host is on this machine or its private network — loopback, link-local,
or an RFC1918 range. The harness confines what the model can reach for the
same reason it scrubs the environment a shell sees: a URL you composed is not
a reason to open a socket into the infrastructure this is running on.

If you meant a public page, check the address. If you need something from a
local service, that is not this tool.
{% endif %}{% if refused-redirect %}`{{url}}` redirected to `{{to}}`, which is on this machine or its private network, so the fetch stopped there.

Checking only the address you gave would be no check at all — a public URL
that redirects inward is exactly how that guard gets walked around.
{% endif %}{% if bad-status %}`{{url}}` answered {{status}}.

Not a harness failure — the page said no. A 404 means the address is wrong, a
403 or 401 means it wants credentials this tool does not carry, and a 5xx
means the far end is unwell and may be worth one retry, not five.
{% endif %}{% if failed %}Could not fetch `{{url}}`: {{detail}}

Read the reason before retrying. A DNS failure will not fix itself; a timeout
might, once.
{% endif %}{% if truncated %}

[cut at {{chars}} characters of {{total}}. Ask for a more specific page rather
than re-fetching this one — the rest of it will not arrive by asking again.]
{% endif %}
