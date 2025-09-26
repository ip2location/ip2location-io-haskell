# Quickstart

## Dependencies

This module requires API key to function. You may sign up for a free API key at <https://www.ip2location.io/pricing>.

Compilation
============
```bash

cabal install ip2location-io

```

## Sample Codes

### Lookup IP Address Geolocation Data

You can make a geolocation data lookup for an IP address as below:

```haskell
import Configuration
import qualified IPGeolocation as IPG
import qualified IPGeolocation as IPAS (ASInfo(..))
import qualified IPGeolocation as IPConti (Continent(..))
import qualified IPGeolocation as IPCntry (Country(..))
import qualified IPGeolocation as IPCurr (Currency(..))
import qualified IPGeolocation as IPLng (Language(..))
import qualified IPGeolocation as IPRgn (Region(..))
import qualified IPGeolocation as IPCty (City(..))

main :: IO ()
main = do
    let apikey = "YOUR_API_KEY"
    let ipadd = "8.8.8.8"
    let language = "" -- language param only supported in Plus and Security plans, put blank if not needed
    config <- open apikey
    result <- IPG.lookUpIP config ipadd language

    case result of
        IPG.IPError err -> putStrLn $ "ERROR: " ++ IPG.error_message (IPG.error err)
        IPG.IPResponse response -> do
            putStrLn $ "ip: " ++ IPG.ip response
            putStrLn $ "country_code: " ++ IPG.country_code response
            putStrLn $ "country_name: " ++ IPG.country_name response
            putStrLn $ "region_name: " ++ IPG.region_name response
            putStrLn $ "city_name: " ++ IPG.city_name response
            putStrLn $ "latitude: " ++ show (IPG.latitude response)
            putStrLn $ "longitude: " ++ show (IPG.longitude response)
            putStrLn $ "zip_code: " ++ IPG.zip_code response
            putStrLn $ "time_zone: " ++ IPG.time_zone response
            putStrLn $ "asn: " ++ IPG.asn response
            putStrLn $ "as: " ++ IPG.as response
            case IPG.as_info response of
                Nothing -> pure ()
                Just asi -> do
                    putStrLn $ "as_info => as_number: " ++ IPAS.as_number asi
                    putStrLn $ "as_info => as_name: " ++ IPAS.as_name asi
                    putStrLn $ "as_info => as_domain: " ++ IPAS.as_domain asi
                    putStrLn $ "as_info => as_usage_type: " ++ IPAS.as_usage_type asi
                    putStrLn $ "as_info => as_cidr: " ++ IPAS.as_cidr asi
            putStrLn $ "isp: " ++ maybe "NOT_SUPPORTED" id (IPG.isp response)
            putStrLn $ "domain: " ++ maybe "NOT_SUPPORTED" id (IPG.domain response)
            putStrLn $ "net_speed: " ++ maybe "NOT_SUPPORTED" id (IPG.net_speed response)
            putStrLn $ "idd_code: " ++ maybe "NOT_SUPPORTED" id (IPG.idd_code response)
            putStrLn $ "area_code: " ++ maybe "NOT_SUPPORTED" id (IPG.area_code response)
            putStrLn $ "weather_station_code: " ++ maybe "NOT_SUPPORTED" id (IPG.weather_station_code response)
            putStrLn $ "weather_station_name: " ++ maybe "NOT_SUPPORTED" id (IPG.weather_station_name response)
            putStrLn $ "mcc: " ++ maybe "NOT_SUPPORTED" id (IPG.mcc response)
            putStrLn $ "mnc: " ++ maybe "NOT_SUPPORTED" id (IPG.mnc response)
            putStrLn $ "mobile_brand: " ++ maybe "NOT_SUPPORTED" id (IPG.mobile_brand response)
            putStrLn $ "elevation: " ++ maybe "NOT_SUPPORTED" (show) (IPG.elevation response)
            putStrLn $ "usage_type: " ++ maybe "NOT_SUPPORTED" id (IPG.usage_type response)
            putStrLn $ "address_type: " ++ maybe "NOT_SUPPORTED" id (IPG.address_type response)
            putStrLn $ "ads_category: " ++ maybe "NOT_SUPPORTED" id (IPG.ads_category response)
            putStrLn $ "ads_category_name: " ++ maybe "NOT_SUPPORTED" id (IPG.ads_category_name response)
            putStrLn $ "district: " ++ maybe "NOT_SUPPORTED" id (IPG.district response)
            putStrLn $ "is_proxy: " ++ show (IPG.is_proxy response)
            putStrLn $ "fraud_score: " ++ maybe "NOT_SUPPORTED" (show) (IPG.fraud_score response)
            case IPG.continent response of
                Nothing -> pure ()
                Just conti -> do
                    putStrLn $ "continent => name: " ++ IPConti.name conti
                    putStrLn $ "continent => code: " ++ IPConti.code conti
                    putStrLn $ "continent => hemispshere: " ++ (show) (IPConti.hemisphere conti)
                    let tran = IPConti.translation conti
                    putStrLn $ "continent => translation => lang: " ++ maybe "NOT_SUPPORTED" id (IPG.lang tran)
                    putStrLn $ "continent => translation => value: " ++ maybe "NOT_SUPPORTED" id (IPG.value tran)
            case IPG.country response of
                Nothing -> pure ()
                Just cntry -> do
                    putStrLn $ "country => name: " ++ IPCntry.name cntry
                    putStrLn $ "country => alpha3_code: " ++ IPCntry.alpha3_code cntry
                    putStrLn $ "country => numeric_code: " ++ show (IPCntry.numeric_code cntry)
                    putStrLn $ "country => demonym: " ++ IPCntry.demonym cntry
                    putStrLn $ "country => flag: " ++ IPCntry.flag cntry
                    putStrLn $ "country => capital: " ++ IPCntry.capital cntry
                    putStrLn $ "country => total_area: " ++ show (IPCntry.total_area cntry)
                    putStrLn $ "country => population: " ++ show (IPCntry.population cntry)
                    putStrLn $ "country => tld: " ++ IPCntry.tld cntry
                    let curr = IPCntry.currency cntry
                    putStrLn $ "country => currency => code: " ++ IPCurr.code curr
                    putStrLn $ "country => currency => name: " ++ IPCurr.name curr
                    putStrLn $ "country => currency => symbol: " ++ IPCurr.symbol curr
                    let lng = IPCntry.language cntry
                    putStrLn $ "country => language => code: " ++ IPLng.code lng
                    putStrLn $ "country => language => name: " ++ IPLng.name lng
                    let tran = IPCntry.translation cntry
                    putStrLn $ "country => translation => lang: " ++ maybe "NOT_SUPPORTED" id (IPG.lang tran)
                    putStrLn $ "country => translation => value: " ++ maybe "NOT_SUPPORTED" id (IPG.value tran)
            case IPG.region response of
                Nothing -> pure ()
                Just rgn -> do
                    putStrLn $ "region => name: " ++ IPRgn.name rgn
                    putStrLn $ "region => code: " ++ IPRgn.code rgn
                    let tran = IPRgn.translation rgn
                    putStrLn $ "region => translation => lang: " ++ maybe "NOT_SUPPORTED" id (IPG.lang tran)
                    putStrLn $ "region => translation => value: " ++ maybe "NOT_SUPPORTED" id (IPG.value tran)
            case IPG.city response of
                Nothing -> pure ()
                Just cty -> do
                    putStrLn $ "city => name: " ++ IPCty.name cty
                    let tran = IPCty.translation cty
                    putStrLn $ "city => translation => lang: " ++ maybe "NOT_SUPPORTED" id (IPG.lang tran)
                    putStrLn $ "city => translation => value: " ++ maybe "NOT_SUPPORTED" id (IPG.value tran)
            case IPG.time_zone_info response of
                Nothing -> pure ()
                Just tzi -> do
                    putStrLn $ "time_zone_info => olson: " ++ IPG.olson tzi
                    putStrLn $ "time_zone_info => current_time: " ++ IPG.current_time tzi
                    putStrLn $ "time_zone_info => gmt_offset: " ++ show (IPG.gmt_offset tzi)
                    putStrLn $ "time_zone_info => is_dst: " ++ show (IPG.is_dst tzi)
                    putStrLn $ "time_zone_info => abbreviation: " ++ IPG.abbreviation tzi
                    putStrLn $ "time_zone_info => dst_start_date: " ++ IPG.dst_start_date tzi
                    putStrLn $ "time_zone_info => dst_end_date: " ++ IPG.dst_end_date tzi
                    putStrLn $ "time_zone_info => sunrise: " ++ IPG.sunrise tzi
                    putStrLn $ "time_zone_info => sunset: " ++ IPG.sunset tzi
            case IPG.geotargeting response of
                Nothing -> pure ()
                Just gtg -> do
                    putStrLn $ "geotargeting => metro: " ++ maybe "NOT_SUPPORTED" id (IPG.metro gtg)
            case IPG.proxy response of
                Nothing -> pure ()
                Just prx -> do
                    putStrLn $ "proxy => last_seen: " ++ show (IPG.last_seen prx)
                    putStrLn $ "proxy => proxy_type: " ++ IPG.proxy_type prx
                    putStrLn $ "proxy => threat: " ++ IPG.threat prx
                    putStrLn $ "proxy => provider: " ++ IPG.provider prx
                    putStrLn $ "proxy => is_vpn: " ++ show (IPG.is_vpn prx)
                    putStrLn $ "proxy => is_tor: " ++ show (IPG.is_tor prx)
                    putStrLn $ "proxy => is_data_center: " ++ show (IPG.is_data_center prx)
                    putStrLn $ "proxy => is_public_proxy: " ++ show (IPG.is_public_proxy prx)
                    putStrLn $ "proxy => is_web_proxy: " ++ show (IPG.is_web_proxy prx)
                    putStrLn $ "proxy => is_web_crawler: " ++ show (IPG.is_web_crawler prx)
                    putStrLn $ "proxy => is_residential_proxy: " ++ show (IPG.is_residential_proxy prx)
                    putStrLn $ "proxy => is_consumer_privacy_network: " ++ show (IPG.is_consumer_privacy_network prx)
                    putStrLn $ "proxy => is_enterprise_private_network: " ++ show (IPG.is_enterprise_private_network prx)
                    putStrLn $ "proxy => is_spammer: " ++ show (IPG.is_spammer prx)
                    putStrLn $ "proxy => is_scanner: " ++ show (IPG.is_scanner prx)
                    putStrLn $ "proxy => is_botnet: " ++ show (IPG.is_botnet prx)
                    putStrLn $ "proxy => is_bogon: " ++ show (IPG.is_bogon prx)
```

### Lookup Domain Information

You can lookup domain information as below:

```haskell
import Configuration
import qualified DomainWhois as DW
import qualified DomainWhois as Regist (Registrar(..))
import qualified DomainWhois as Contac (Contact(..))

main :: IO ()
main = do
    let apikey = "YOUR_API_KEY"
    let mydomain = "locaproxy.com"
    config <- open apikey
    result <- DW.lookUpDomain config mydomain

    case result of
        DW.WhoisError err -> putStrLn $ "ERROR: " ++ DW.error_message (DW.error err)
        DW.WhoisResponse response -> do
            putStrLn $ "domain: " ++ DW.domain response
            putStrLn $ "domain_id: " ++ DW.domain_id response
            putStrLn $ "status: " ++ DW.status response
            putStrLn $ "create_date: " ++ DW.create_date response
            putStrLn $ "update_date: " ++ DW.update_date response
            putStrLn $ "expire_date: " ++ DW.expire_date response
            putStrLn $ "domain_age: " ++ show (DW.domain_age response)
            putStrLn $ "whois_server: " ++ DW.whois_server response

            let registrarObj = DW.registrar response
            putStrLn $ "registrar => iana_id: " ++ Regist.iana_id registrarObj
            putStrLn $ "registrar => name: " ++ Regist.name registrarObj
            putStrLn $ "registrar => url: " ++ Regist.url registrarObj

            let registrantObj = DW.registrant response
            putStrLn $ "registrant => name: " ++ Contac.name registrantObj
            putStrLn $ "registrant => organization: " ++ Contac.organization registrantObj
            putStrLn $ "registrant => street_address: " ++ Contac.street_address registrantObj
            putStrLn $ "registrant => city: " ++ Contac.city registrantObj
            putStrLn $ "registrant => region: " ++ Contac.region registrantObj
            putStrLn $ "registrant => zip_code: " ++ Contac.zip_code registrantObj
            putStrLn $ "registrant => country: " ++ Contac.country registrantObj
            putStrLn $ "registrant => phone: " ++ Contac.phone registrantObj
            putStrLn $ "registrant => fax: " ++ Contac.fax registrantObj
            putStrLn $ "registrant => email: " ++ Contac.email registrantObj

            let adminObj = DW.admin response
            putStrLn $ "admin => name: " ++ Contac.name adminObj
            putStrLn $ "admin => organization: " ++ Contac.organization adminObj
            putStrLn $ "admin => street_address: " ++ Contac.street_address adminObj
            putStrLn $ "admin => city: " ++ Contac.city adminObj
            putStrLn $ "admin => region: " ++ Contac.region adminObj
            putStrLn $ "admin => zip_code: " ++ Contac.zip_code adminObj
            putStrLn $ "admin => country: " ++ Contac.country adminObj
            putStrLn $ "admin => phone: " ++ Contac.phone adminObj
            putStrLn $ "admin => fax: " ++ Contac.fax adminObj
            putStrLn $ "admin => email: " ++ Contac.email adminObj

            let techObj = DW.tech response
            putStrLn $ "tech => name: " ++ Contac.name techObj
            putStrLn $ "tech => organization: " ++ Contac.organization techObj
            putStrLn $ "tech => street_address: " ++ Contac.street_address techObj
            putStrLn $ "tech => city: " ++ Contac.city techObj
            putStrLn $ "tech => region: " ++ Contac.region techObj
            putStrLn $ "tech => zip_code: " ++ Contac.zip_code techObj
            putStrLn $ "tech => country: " ++ Contac.country techObj
            putStrLn $ "tech => phone: " ++ Contac.phone techObj
            putStrLn $ "tech => fax: " ++ Contac.fax techObj
            putStrLn $ "tech => email: " ++ Contac.email techObj

            let billingObj = DW.billing response
            putStrLn $ "billing => name: " ++ Contac.name billingObj
            putStrLn $ "billing => organization: " ++ Contac.organization billingObj
            putStrLn $ "billing => street_address: " ++ Contac.street_address billingObj
            putStrLn $ "billing => city: " ++ Contac.city billingObj
            putStrLn $ "billing => region: " ++ Contac.region billingObj
            putStrLn $ "billing => zip_code: " ++ Contac.zip_code billingObj
            putStrLn $ "billing => country: " ++ Contac.country billingObj
            putStrLn $ "billing => phone: " ++ Contac.phone billingObj
            putStrLn $ "billing => fax: " ++ Contac.fax billingObj
            putStrLn $ "billing => email: " ++ Contac.email billingObj

            putStrLn $ "nameservers: " ++ (show) (DW.nameservers response)
```

### Get Domain Name

You can extract the domain name from an url as below:

```haskell
import qualified DomainWhois as DW

main :: IO ()
main = do
    let domainName = DW.getDomainName "https://www.example.com/exe"
    putStrLn $ "domainName: " ++ domainName
```

### Get Domain Extension

You can extract the domain extension from a domain name or url as below:

```haskell
import qualified DomainWhois as DW

main :: IO ()
main = do
    let domainExtension = DW.getDomainExtension "example.com"
    putStrLn $ "domainExtension: " ++ domainExtension
```

### Lookup IP Address Hosted Domains Data

You can lookup hosted domains information as below:

```haskell
import Configuration
import qualified HostedDomain as HD

main :: IO ()
main = do
    let apikey = "YOUR_API_KEY"
    let ipadd = "8.8.8.8"
    let page = 1
    config <- open apikey
    result <- HD.lookUpHosted config ipadd page

    case result of
        HD.HostedError err -> putStrLn $ "ERROR: " ++ HD.error_message (HD.error err)
        HD.HostedResponse response -> do
            putStrLn $ "ip: " ++ HD.ip response
            putStrLn $ "total_domains: " ++ (show) (HD.total_domains response)
            putStrLn $ "page: " ++ (show) (HD.page response)
            putStrLn $ "per_page: " ++ (show) (HD.per_page response)
            putStrLn $ "total_pages: " ++ (show) (HD.total_pages response)
            putStrLn $ "domains: " ++ (show) (HD.domains response)
```