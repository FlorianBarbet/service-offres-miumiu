# Creer Entreprise

POST /entreprise
```json
head : {"jwt":"Un token valide"}
body :
{
	"libelle":"Ma Petite Entreprise",
	"numero":"5 Bis",
	"rue":"avenue Jean-Jaurès",
	"code_postal":"59800",
	"ville":"LILLE"
}

response : 201 | 400 | 403
```
# Creer type contrat

POST /contrat
```json
head : {"jwt":"Un token valide"}
body :
{
	"sigle":"CDD",
	"description":"Contrat a duree determine"
}

response : 201 | 400 | 403
```

# Creer Offre

POST /offre/:id_entreprise/:id_contrat
```json
head : {"jwt":"Un token valide"}
body :
 {
	"titre":"Tech Lead Java H/F - Norsys",
	"description":"Bienvenido",
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
```json
head : {"jwt":"Un token valide"}
body: 
{
	"titre":"Tech Lead OCaml H/F - Norsys",
	"description":"Un choix de roi ...",
	"created_at":"2021-03-10",
	"end_at":"2021-03-21",
	"id_contrat":"a465b0d7-05e9-4a86-bdaf-0f7ee09b4137",
	"contact":"toto@norsys.fr",
	"duree" : null
}

response : 201 | 400 | 403
```

-> Le body contient les éléments à modifier il peut donc être similaire à la création si on compte tout modifier

# Supprimer Offre.

DELETE /offre/:id_offre
```json
head : {"jwt":"Un token valide"}

response : 200 | 400 | 403
```

# Réactiver une offre

PUT /disable-offre/a465b0d7-05e9-4a86-bdaf-0f7ee09b4137
```json
head : {"jwt":"Un token valide"}

response : 200 | 400 | 403
```

# Afficher liste des offres actives pour une ville

GET /offre/list/:ville

response example: 
id is composed of ranking#id_offre
```json
[
    "5.47945205479e-05#61e01881-e7a1-46ac-93b4-a8017ba25601": {
        "id": "61e01881-e7a1-46ac-93b4-a8017ba25601",
        "titre": "CDD Agent d'entretien - Inetum",
        "description": "Avancer avec nous passe par l'hygiène absolue !",
        "created_at": "1980-03-15",
        "end_at": "2030-03-15",
        "entreprise": {
            "id": "15ef1436-3857-4e4c-8dab-cc7b76be1a98",
            "libelle": "Inetum_",
            "description": "Un pas en avant pour le latin !",
            "numero": "17",
            "rue": "RUE EDOUARD DELESALLE",
            "code_postal": 59800,
            "ville": "LILLE"
        },
        "contrat": {
            "id": "23488ef9-dafd-4340-a4da-a2e0b0a44b8e",
            "sigle": "CDD",
            "description": "Contrat a Duree Determinee"
        },
        "contact":"toto@toto.fr",
        "duree": 18
    },...]
```

# Afficher le detail d'une offre

GET /offre/detail/:id_offre
 
response example: 
```json
{
    "id": "a465b0d7-05e9-4a86-bdaf-0f7ee09b4137",
    "titre": "CDD Agent d'entretien - OhNo",
    "description": "Oula j'ai modifié",
    "created_at": "1980-03-15",
    "end_at": "2030-03-15",
    "entreprise": {
        "id": "202789de-ef8c-4dc9-9c2c-69591daf9c56",
        "libelle": "OhNo",
        "description": "My bad !",
        "numero": "17",
        "rue": "RUE EDOUARD DELESALLE",
        "code_postal": 59800,
        "ville": "LILLE"
    },
    "contrat": {
        "id": "23488ef9-dafd-4340-a4da-a2e0b0a44b8e",
        "sigle": "CDI",
        "description": "CDI"
    },
    "contact":"toto@toto.fr"
}
```

# Afficher les villes

GET /offre/villes

response :
```json
{
    "villes": [
        "LILLE"
    ]
}
```

# Afficher les entreprises

GET /entreprise

response :

```json
{
	[
        {
            "id": "15ef1436-3857-4e4c-8dab-cc7b76be1a98",
            "libelle": "Netflix",
            "description": "On chill devant Cobra Kai ?",
            "numero": "17",
            "rue": "RUE EDOUARD DELESALLE",
            "code_postal": 59800,
            "ville": "LILLE"
        },...
    ]
}
```

# Afficher les types de contrats 

GET /contrat

response :

```json
{
    "23488ef9-dafd-4340-a4da-a2e0b0a44b8e": {
        "id": "23488ef9-dafd-4340-a4da-a2e0b0a44b8e",
        "sigle": "CDD",
        "description": "Contrat a Duree Determine"
    },
	....
}
```

# Lister les offres désactivées

GET /offre/disable

```json
head : {"jwt":"Un token valide"}
response :
{
    "a465b0d7-05e9-4a86-bdaf-0f7ee09b4137": {
        "id": "a465b0d7-05e9-4a86-bdaf-0f7ee09b4137",
        "titre": "CDD Agent d'entretien - Inetum",
        "entreprise": {
            "id": "202789de-ef8c-4dc9-9c2c-69591daf9c56",
            "libelle": "Ha__la",
            "description": "Hallloha !",
            "numero": "17",
            "rue": "RUE EDOUARD DELESALLE",
            "code_postal": 59800,
            "ville": "LILLE"
        },
        "contrat": {
            "id": "23488ef9-dafd-4340-a4da-a2e0b0a44b8e",
            "sigle": "CDD",
            "description": "CDD"
        },
        "created_at": "1980-03-15",
        "end_at": "2030-03-15"
    }
}
```

# Liste les offres du membre

GET /offre
```json
head : {"jwt":"Un token valide"}
response :
{
    "a465b0d7-05e9-4a86-bdaf-0f7ee09b4137": {
        "id": "a465b0d7-05e9-4a86-bdaf-0f7ee09b4137",
        "titre": "CDD Agent d'entretien - Inetum",
        "description": "Oula j'ai modifié",
        "created_at": "1980-03-15",
        "end_at": "2030-03-15",
        "entreprise": {
            "id": "202789de-ef8c-4dc9-9c2c-69591daf9c56",
            "libelle": "Ahyaya____la",
            "description": "TOTO",
            "numero": "17",
            "rue": "RUE EDOUARD DELESALLE",
            "code_postal": 59800,
            "ville": "LILLE"
        },
        "contrat": {
            "id": "23488ef9-dafd-4340-a4da-a2e0b0a44b8e",
            "sigle": "CDI",
            "description": "CDI"
        },
        "contact":"toto@toto.fr"
    }
}
```