Your edit to `{{path}}` was REJECTED: the {{kind}} `{{role}}` failed its {{stage}} check{% if line %} at line {{line}}{% if column %}, column {{column}}{% endif %}{% endif %}:

    {{message}}

The file still holds what you wrote, but the previous version is still what runs — nothing you changed in it is live. Fix it in place: read the file, correct the problem above, and write it again. It is checked again on the next read and takes effect the moment it passes.
