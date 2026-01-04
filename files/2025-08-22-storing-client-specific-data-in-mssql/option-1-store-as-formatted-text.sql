-- Store client-specific data as formatted text in a single column
-- This approach allows for flexibility in storing different data structures for each country

-- Create the Customs Declarations table
CREATE TABLE dbo.declarations
(
    id                    INT IDENTITY (1,1) PRIMARY KEY,
    country               VARCHAR(2)   NOT NULL,
    status                VARCHAR(32)  NOT NULL,
    country_specific_data VARCHAR(400) NOT NULL
);

-- Insert sample data for different countries
INSERT INTO dbo.declarations (country, status, country_specific_data)
VALUES ('VN', 'Pending', 'country=VN,certificate_of_origin=CO-12345,special_handling=perishable'),
       ('US', 'Cleared', 'country=US,certificate_of_origin=US-99999,special_handling=none'),
       ('VN', 'Pending', 'country=VN,special_handling=perishable'); -- no certificate_of_origin

-- Query to extract the certificate_of_origin for each declaration
SELECT d.id,
       d.country,
       d.status,
       certificate_of_origin =
           CONVERT(VARCHAR(35), CASE
                                    WHEN CHARINDEX(',certificate_of_origin=', ',' + country_specific_data) > 0
                                        THEN REPLACE(
                                            SUBSTRING(',' + country_specific_data,
                                                      CHARINDEX(',certificate_of_origin=', '*' + country_specific_data) +
                                                      23,
                                                      CHARINDEX(',', ',' + country_specific_data + ',',
                                                                CHARINDEX(',certificate_of_origin=', ',' + country_specific_data) +
                                                                1) -
                                                      (CHARINDEX(',certificate_of_origin=', ',' + country_specific_data) + 23)),
                                            '¤', ',')
                                    ELSE '' END)
FROM dbo.declarations AS d