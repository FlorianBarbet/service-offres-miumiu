

CREATE TABLE "Entreprise"(
    id UUID PRIMARY KEY NOT NULL,
    libelle TEXT NOT NULL,
    description TEXT NOT NULL,
    
    numero TEXT NOT NULL,
    rue TEXT NOT NULL,
    code_postal integer NOT NULL,
    ville TEXT NOT NULL
);


CREATE TABLE "Contrat"(
    id UUID PRIMARY KEY NOT NULL,
    sigle TEXT,
    description TEXT
);

CREATE TABLE "Offre"
(
    id UUID PRIMARY KEY NOT NULL,
    titre TEXT NOT NULL,
    description TEXT NOT NULL,
    created_at DATE NOT NULL DEFAULT CURRENT_DATE,
    end_at DATE NOT NULL,

    id_entreprise UUID NOT NULL,
    id_contrat UUID NOT NULL,
    membre_id UUID NOT NULL,

    contact TEXT NOT NULL,
    duree integer,
    active boolean NOT NULL DEFAULT true,
       
    CONSTRAINT fk_coe_entreprise 
       FOREIGN KEY (id_entreprise) REFERENCES "Entreprise"(id),
    CONSTRAINT fk_coe_contrat 
       FOREIGN KEY (id_contrat) REFERENCES "Contrat"(id)
);

/*

We've seperated tables into a ternaire association,
That's would give more optimisation.

This is not checkable with some request it's an optimisation in term of charge 
Data Manipulation put a lock while transaction if we dont seperate tables it could lock some peoples 
if there is more transaction than possible usage

Like that each entity will be independant !

*/