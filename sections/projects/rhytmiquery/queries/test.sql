SELECT
    alb.Title AS Album,
    art.Name AS Artist
FROM
    albums AS alb
    INNER JOIN artists AS art ON art.artistID = alb.artistID
ORDER BY
    artist;