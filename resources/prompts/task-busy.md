You are already working on {{current-id}} — {{current-title}}. {% if board %}It is
the board's work, so finish it by calling `done`; the board closes it when your
diff passes review.{% else %}Finish it with `task close {{current-id}}`.{% endif %} If
you genuinely need to change what you are working on, use `task switch` with a
`reason` — switching is recorded, so say why the current task is being set down.
