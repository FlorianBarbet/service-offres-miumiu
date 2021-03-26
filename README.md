# TODO mettre à jour

# Creer Entreprise

POST /entreprise
```
head : {"jwt":"Un token valide"}
{
	"libelle":"Ma Petite Entreprise",
	"numero":"5 Bis"
	"rue":"avenue Jean-Jaurès",
	"code_postal":"59800",
	"ville":"LILLE"
}

response : 201 | 400 | 403
```
# Creer type contrat

POST /contrat
```
head : {"jwt":"Un token valide"}
{
		"sigle":"CDD",
		"description":"Contrat a duree determine"
}

response : 201 | 400 | 403
```

# Creer Offre

POST /offre/:id_entreprise/:id_contrat
```
head : {"jwt":"Un token valide"}
body : {
"titre":"Tech Lead Java H/F - Norsys",
"description":"Lors de cette mission vous allez être confronté à ...",
"created_at":"2021-03-10",
"end_at":"2021-03-12",
"duree":18,
"contact":"toto@norsys.fr"
}

la duree est optionnel

response : 201 | 400 | 403
```

/!\ 
-> le format date sera toujours : dd-MM-yyyy

# Modifier Offre

PUT /offre/:id_offre
```
head : {"jwt":"Un token valide"}
{
"titre":"Tech Lead OCaml H/F - Norsys",
"description":"Un choix de roi ...",
"created_at":"2021-03-10",
"end_at":"2021-03-21",
"id_contrat":"a465b0d7-05e9-4a86-bdaf-0f7ee09b4137",
"contact":"toto@norsys.fr"
"duree":null
}

response : 201 | 400 | 403
```

-> Le body contient les éléments à modifier il peut donc être similaire à la création si on compte tout modifier

# Supprimer Offre.

DELETE /offre/:id_offre
```
head : {"jwt":"Un token valide"}
```

# Afficher liste des offres actives pour une ville

GET /offre/list/:ville

response: 
```
[{
	"id_offre":"1",
	"titre":"Tech Lead Java H/F - Norsys",
	"description":"Lors de cette mission vous allez être confronté à ...",
	"created_at":"13-03-2021",
	"end_at":"14-03-2021",
	"entreprise":{
		"id":"1",
		"libelle":"Norsys",
		"numero":"5 Bis"
		"rue":"avenue Jean-Jaurès",
		"code_postal":"59800",
		"ville":"LILLE"
	},
	"contrat":{
		"sigle":"CDD",
		"description":"Contrat a duree determine"
	},
	"contact":"toto@norsys.fr",
	"duree":18
},
{
...
}]
```

# Afficher le detail d'une offre

GET /offre/detail/:id_offre
 
response: 
```
{
	"id_offre":"1",
	"titre":"Tech Lead Java H/F - Norsys",
	"description":"Lors de cette mission vous allez être confronté à ...",
	"created_at":"13-03-2021",
	"end_at":"14-03-2021",
	"entreprise":{
		"id":"1",
		"libelle":"Norsys",
		"numero":"5 Bis"
		"rue":"avenue Jean-Jaurès",
		"code_postal":"59800",
		"ville":"LILLE"
	},
	"contrat":{
		"sigle":"CDD",
		"description":"Contrat a duree determine"
	},
	"contact":"toto@norsys.fr",
	"duree":18
}
```
