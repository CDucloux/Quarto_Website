SELECT
    art.Name AS ArtistName,
    COUNT(alb.albumID) AS NumberOfAlbums
FROM
    albums AS alb
    INNER JOIN artists AS art ON alb.artistID = art.artistID
GROUP BY
    ArtistName
ORDER BY
    COUNT(alb.albumID) DESC