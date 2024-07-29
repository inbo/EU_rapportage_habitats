# EU_rapportage_habitats

Optimalisatie van het EU-rapportageproces habitats t.b.v. de 2025 rapportage

## Voorbereidende stappen

### Inlezen data

Via het bestandje **lees_data.R** wordt de informatie ingelezen uit de habitatfiches en BijlageHabitatKwaliteit

Voor ieder hoofdstuk (gaat telkens over een ander landschapsstype) wordt een docx template gemaakt. Via het script **creer_lege_paragrafen_habitatauteurs.R** Deze moeten de wetenschappers aanvullen

### Template documenten

Eerst worden instructies gemaakt voor de wetenschappers. Deze zijn pdf-documenten die gerenderd worden in het script **creer_lege_paragrafen_habitatauteurs** die het gelijknamige .Rmd bestand zal gebruiken als bron

Daarnaast worden docx templates gemaakt per hoofdstuk (onder data/raw), waarin de wetenschappers dan hun tekst in kwijt kunnen (de instructies zeggen hoe ze moeten refereren naar de figuren). Moesten die kwijt geraakt worden. De aangepaste docx bestanden van de wetenschappers worden dan terug naar markdown omgezet.

De huidige structuur van de docx bestanden zijn:

```         
# Handleiding:

Schrijf hier onder ieder van de koppen je tekst die bij de bijbehorende subsectie hoort. 

- Wijzig geen enkele van de hoofdingen in dit document; maak er geen bij, doe er geen weg en behoud de huidige benamingen
- Indien je tekst in italics wil schrijven, doe je dat door voor en na dit stuk tekst een ‘*’asterisk’*’ te zetten.
- Voor bold tekst, zet je twee ‘**’asterisken’**’ voor en na je stuk tekst dat je vet gedrukt wil zien in het document
- Volg de aanwijzingen in het pdf document voor referenties naar figuren en tabellen

#Inleiding

Schrijf hier de tekst voor de inleiding.

# Areaal

Schrijf hier de tekst voor het areaal

# Oppervlakte

Schrijf hier de tekst voor de oppervlakte

# Regionale_toestand

Schrijf hier de tekst voor de regionale toestand

# Drukken_bedreigingen

Schrijf hier de tekst voor drukken en bedreigingen

# Instandhoudingsmaatregelen

Schrijf hier de tekst voor instandhoudingsmaatregelen

# Toekomstperspectieven

Schrijf hier de tekst voor toekomstperspectieven

# Conclusies

Schrijf hier de tekst voor de conclusie
```

## Data incorporeren

Als de wetenschappers hun docx hebben aangevuld wordt dit omgezet naar een markdown document per paragraaf. Dit gebeurt in het script **habitatauteurd_from-docx**

Deze teksten worden dan gecombineerd met alle figuren en tabellen die gemaakt worden via **\_template_habitats.Rmd**

## Rapport renderen

Alle teksten, figuren en tabellen die per hoofdstuk gemaakt zijn worden geïntegreerd in 1 groot pdf-rapport, door alle kleine markdown bestandjes te combineren via een bookdown-rapport.
