using Documenter, DocumenterMarkdown, PermGroups

makedocs(sitename="PermGroups documentation",format=Markdown(),modules=[PermGroups, Perms, Groups])
