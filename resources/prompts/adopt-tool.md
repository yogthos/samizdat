{% if listing %}{% if offers %}On offer to this project — nothing here is applied until you take it:
{% for o in offers %}
  {{o.kind}} {{o.name}}  {{o.offer}}{% if o.edited %} (this project edited its copy too){% endif %}{% if o.version %} v{{o.version}}{% endif %}  {{o.path}}{% endfor %}

new: a role the shipped workflow has and this project does not. updated: the shipped template changed since this project saw it. pending: a version this project stored before its workflow lived in files. `adopt show` one before you decide; take what helps this project, decline the rest with the reason.{% else %}Nothing on offer: this project has answered every template it has been shown.{% endif %}{% endif %}{% if showing %}{{kind}} {{name}} ({{offer}}{% if edited %}; this project edited its copy too{% endif %}) — offered text:

{{text}}{% if current %}

This project's current {{path}}:

{{current}}{% endif %}{% endif %}{% if adopted %}Adopted {{kind}} {{name}} ({{offer}}) as v{{version}}; .samizdat/{{path}} is what runs now.{% endif %}{% if refused %}{{kind}} {{name}} was NOT adopted: the offered text fails its check at {{stage}}{% if line %} (line {{line}}{% if column %}, column {{column}}{% endif %}){% endif %}: {{message}}
It is still on offer. A manifest that names a cell this project lacks needs that cell adopted first.{% endif %}{% if declined %}Declined {{kind}} {{name}} ({{offer}}). It will not be offered again until a later release changes it.{% endif %}{% if no-offer %}Nothing on offer for {{kind}} {{name}}. `adopt list` shows what is.{% endif %}{% if bad-kind %}Unknown kind '{{kind}}': one of manifest, prompt, policy, cell.{% endif %}{% if unknown-action %}Unknown `adopt` action '{{action}}'. {{usage}}{% endif %}{% if needs-action %}`adopt` needs an `action`. {{usage}}{% endif %}
