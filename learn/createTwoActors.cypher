CREATE (a:Person:Actor {name:"Tom Hanks", born:1956})
        -[r:ACTED_IN {roles: ["Forrest"]}]->
       (m:Movie {title:"Forrest Gump",released:1994})

CREATE (d:Person:Director {name:"Robert Zemeckis", born:1951})-[:DIRECTED]->(m)
CREATE (e:Person:Director {name:"Jordan Peel", born:1973})-[:DIRECTED]->(m)
;; MATCH (n) set n.id = id(n)
RETURN a,d,r,m
