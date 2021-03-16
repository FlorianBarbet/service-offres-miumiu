# Creer Entreprise

POST /entreprise
head : {"jwt":"Un token valide"}
{
	"libelle":"Norsys",
	"numero":"5 Bis"
	"rue":"avenue Jean-Jaurès",
	"code_postal":"59800",
	"ville":"LILLE"
}

# Creer type contrat

POST /contrat
head : {"jwt":"Un token valide"}
{
		"sigle":"CDD",
		"description":"Contrat a duree determine"
}

# Creer Offre

POST /offre/:id_entreprise/:sigle_contrat
head : {"jwt":"Un token valide"}
body : {
"titre":"Tech Lead Java H/F - Norsys",
"description":"Lors de cette mission vous allez être confronté à ...",
"created_at":"13-03-2021",
"end_at":"14-03-2021",
"duree":18,
"contact":"toto@norsys.fr"
}

/!\ 
-> le format date sera toujours : dd-MM-yyyy

# Modifier Offre

PUT /offre/:id_offre
head : {"jwt":"Un token valide"}
{
	"titre":"Tech Lead OCaml H/F - Norsys",
    "end_at":"22-06-2021"
}

-> Le body contient les éléments à modifier il peut donc être similaire à la création si on compte tout modifier

# Supprimer Offre.

DELETE /offre/:id_offre
head : {"jwt":"Un token valide"}


# Afficher liste des offres actives pour une ville

GET /offre/:ville

response: 
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

# Afficher le detail d'une offre

GET /offre/:id_offre
 
response: 
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