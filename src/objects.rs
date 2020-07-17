use chrono::{Datelike, NaiveDate, Weekday};
use rgb::RGB8;
use serde::de::{self, Deserialize, Deserializer};
use serde::ser::{Serialize, Serializer};
use std::fmt;
use std::sync::Arc;

pub trait Id {
    fn id(&self) -> &str;
}

pub trait Type {
    fn object_type(&self) -> ObjectType;
}

#[derive(Debug, Serialize, Eq, PartialEq, Hash)]
pub enum ObjectType {
    Agency,
    Stop,
    Route,
    Trip,
    Calendar,
    Shape,
    Fare,
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize)]
pub enum LocationType {
    StopPoint = 0,
    StopArea = 1,
    StationEntrance = 2,
    GenericNode = 3,
    BoardingArea = 4,
}

impl<'de> Deserialize<'de> for LocationType {
    fn deserialize<D>(deserializer: D) -> Result<LocationType, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s: String = String::deserialize(deserializer)?;
        Ok(match s.as_str() {
            "1" => LocationType::StopArea,
            "2" => LocationType::StationEntrance,
            "3" => LocationType::GenericNode,
            "4" => LocationType::BoardingArea,
            _ => LocationType::StopPoint,
        })
    }
}

impl Default for LocationType {
    fn default() -> LocationType {
        LocationType::StopPoint
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum RouteType {
    Tramway,
    Subway,
    Rail,
    Bus,
    Ferry,
    CableCar,
    Gondola,
    Funicular,
    // extended GTFS (https://developers.google.com/transit/gtfs/reference/extended-route-types)
    Coach,
    Air,
    Taxi,
    Other(u16),
}

impl RouteType {
    /// Map values according to https://developers.google.com/transit/gtfs/reference/extended-route-types
    pub fn normalized(&self) -> Self {
        match self {
            Self::Other(100) => Self::Rail,                 // Railway Service
            Self::Other(101) => Self::Rail,                 // High Speed Rail Service
            Self::Other(102) => Self::Rail,                 // Long Distance Trains
            Self::Other(103) => Self::Rail,                 // Inter Regional Rail Service
            Self::Other(104) => Self::Rail,                 // Car Transport Rail Service
            Self::Other(105) => Self::Rail,                 // Sleeper Rail Service
            Self::Other(106) => Self::Rail,                 // Regional Rail Service
            Self::Other(107) => Self::Rail,                 // Tourist Railway Service
            Self::Other(108) => Self::Rail,                 // Rail Shuttle (Within Complex)
            Self::Other(109) => Self::Rail,                 // Suburban Railway
            Self::Other(110) => Self::Rail,                 // Replacement Rail Service
            Self::Other(111) => Self::Rail,                 // Special Rail Service
            Self::Other(112) => Self::Rail,                 // Lorry Transport Rail Service
            Self::Other(113) => Self::Rail,                 // All Rail Services
            Self::Other(114) => Self::Rail,                 // Cross-Country Rail Service
            Self::Other(115) => Self::Rail,                 // Vehicle Transport Rail Service
            Self::Other(116) => Self::Rail,                 // Rack and Pinion Railway
            Self::Other(117) => Self::Rail,                 // Additional Rail Service
            Self::Other(200) => Self::Coach,                // Coach Service
            Self::Other(201) => Self::Coach,                // International Coach Service
            Self::Other(202) => Self::Coach,                // National Coach Service
            Self::Other(203) => Self::Coach,                // Shuttle Coach Service
            Self::Other(204) => Self::Coach,                // Regional Coach Service
            Self::Other(205) => Self::Coach,                // Special Coach Service
            Self::Other(206) => Self::Coach,                // Sightseeing Coach Service
            Self::Other(207) => Self::Coach,                // Tourist Coach Service
            Self::Other(208) => Self::Coach,                // Commuter Coach Service
            Self::Other(209) => Self::Coach,                // All Coach Services
            Self::Other(400) => *self,                      // Urban Railway Service
            Self::Other(401) => Self::Subway,               // Metro Service
            Self::Other(402) => Self::Subway,               // Underground Service
            Self::Other(403) => *self,                      // Urban Railway Service
            Self::Other(404) => *self,                      // All Urban Railway Services
            Self::Other(405) => *self,                      // Monorail
            Self::Other(700) => Self::Bus,                  // Bus Service
            Self::Other(701) => Self::Bus,                  // Regional Bus Service
            Self::Other(702) => Self::Bus,                  // Express Bus Service
            Self::Other(703) => Self::Bus,                  // Stopping Bus Service
            Self::Other(704) => Self::Bus,                  // Local Bus Service
            Self::Other(705) => Self::Bus,                  // Night Bus Service
            Self::Other(706) => Self::Bus,                  // Post Bus Service
            Self::Other(707) => Self::Bus,                  // Special Needs Bus
            Self::Other(708) => Self::Bus,                  // Mobility Bus Service
            Self::Other(709) => Self::Bus,                  // Mobility Bus for Registered Disabled
            Self::Other(710) => Self::Bus,                  // Sightseeing Bus
            Self::Other(711) => Self::Bus,                  // Shuttle Bus
            Self::Other(712) => Self::Bus,                  // School Bus
            Self::Other(713) => Self::Bus,                  // School and Public Service Bus
            Self::Other(714) => Self::Bus,                  // Rail Replacement Bus Service
            Self::Other(715) => Self::Bus,                  // Demand and Response Bus Service
            Self::Other(716) => Self::Bus,                  // All Bus Services
            Self::Other(800) => Self::Tramway,              // Trolleybus Service
            Self::Other(900) => Self::Tramway,              // Tram Service
            Self::Other(901) => Self::Tramway,              // City Tram Service
            Self::Other(902) => Self::Tramway,              // Local Tram Service
            Self::Other(903) => Self::Tramway,              // Regional Tram Service
            Self::Other(904) => Self::Tramway,              // Sightseeing Tram Service
            Self::Other(905) => Self::Tramway,              // Shuttle Tram Service
            Self::Other(906) => Self::Tramway,              // All Tram Services
            Self::Other(1000) => Self::Ferry,               // Water Transport Service
            Self::Other(1100) => Self::Air,                 // Air Service
            Self::Other(1200) => Self::Ferry,               // Ferry Service
            Self::Other(1300) => Self::Gondola,             // Aerial Lift Service
            Self::Other(1400) => Self::Funicular,           // Funicular Service
            Self::Other(1500) => Self::Taxi,                // Taxi Service
            Self::Other(1501) => Self::Taxi,                // Communal Taxi Service
            Self::Other(1502) => Self::Taxi,                // Water Taxi Service
            Self::Other(1503) => Self::Taxi,                // Rail Taxi Service
            Self::Other(1504) => Self::Taxi,                // Bike Taxi Service
            Self::Other(1505) => Self::Taxi,                // Licensed Taxi Service
            Self::Other(1506) => Self::Taxi,                // Private Hire Service Vehicle
            Self::Other(1507) => Self::Taxi,                // All Taxi Services
            Self::Other(1700) => *self,                     // Miscellaneous Service
            Self::Other(1702) => *self,                     // Horse-drawn Carriage
            _ => *self
        }
    }
}

impl Default for RouteType {
    fn default() -> RouteType {
        RouteType::Bus
    }
}

impl<'de> Deserialize<'de> for RouteType {
    fn deserialize<D>(deserializer: D) -> Result<RouteType, D::Error>
    where
        D: Deserializer<'de>,
    {
        let i = u16::deserialize(deserializer)?;

        let hundreds = i / 100;
        Ok(match (i, hundreds) {
            (0, _) | (_, 9) => RouteType::Tramway,
            (1, _) | (_, 4) => RouteType::Subway,
            (2, _) | (_, 1) => RouteType::Rail,
            (3, _) | (_, 7) | (_, 8) => RouteType::Bus,
            (4, _) | (_, 10) | (_, 12) => RouteType::Ferry,
            (5, _) => RouteType::CableCar,
            (6, _) | (_, 13) => RouteType::Gondola,
            (7, _) | (_, 14) => RouteType::Funicular,
            (_, 2) => RouteType::Coach,
            (_, 11) => RouteType::Air,
            (_, 15) => RouteType::Taxi,
            _ => RouteType::Other(i),
        })
    }
}

impl Serialize for RouteType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Note: for extended route type, we might loose the initial precise route type
        serializer.serialize_u16(match self {
            RouteType::Tramway => 0,
            RouteType::Subway => 1,
            RouteType::Rail => 2,
            RouteType::Bus => 3,
            RouteType::Ferry => 4,
            RouteType::CableCar => 5,
            RouteType::Gondola => 6,
            RouteType::Funicular => 7,
            RouteType::Coach => 200,
            RouteType::Air => 1100,
            RouteType::Taxi => 1500,
            RouteType::Other(i) => *i,
        })
    }
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
#[derive(Debug, Serialize, Deserialize, Copy, Clone, PartialEq)]
pub enum PickupDropOffType {
    #[derivative(Default)]
    #[serde(rename = "0")]
    Regular,
    #[serde(rename = "1")]
    NotAvailable,
    #[serde(rename = "2")]
    ArrangeByPhone,
    #[serde(rename = "3")]
    CoordinateWithDriver,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Calendar {
    #[serde(rename = "service_id")]
    pub id: String,
    #[serde(
        deserialize_with = "deserialize_bool",
        serialize_with = "serialize_bool"
    )]
    pub monday: bool,
    #[serde(
        deserialize_with = "deserialize_bool",
        serialize_with = "serialize_bool"
    )]
    pub tuesday: bool,
    #[serde(
        deserialize_with = "deserialize_bool",
        serialize_with = "serialize_bool"
    )]
    pub wednesday: bool,
    #[serde(
        deserialize_with = "deserialize_bool",
        serialize_with = "serialize_bool"
    )]
    pub thursday: bool,
    #[serde(
        deserialize_with = "deserialize_bool",
        serialize_with = "serialize_bool"
    )]
    pub friday: bool,
    #[serde(
        deserialize_with = "deserialize_bool",
        serialize_with = "serialize_bool"
    )]
    pub saturday: bool,
    #[serde(
        deserialize_with = "deserialize_bool",
        serialize_with = "serialize_bool"
    )]
    pub sunday: bool,
    #[serde(
        deserialize_with = "deserialize_date",
        serialize_with = "serialize_date"
    )]
    pub start_date: NaiveDate,
    #[serde(
        deserialize_with = "deserialize_date",
        serialize_with = "serialize_date"
    )]
    pub end_date: NaiveDate,
}

impl Type for Calendar {
    fn object_type(&self) -> ObjectType {
        ObjectType::Calendar
    }
}

impl Id for Calendar {
    fn id(&self) -> &str {
        &self.id
    }
}

impl fmt::Display for Calendar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}—{}", self.start_date, self.end_date)
    }
}

#[derive(Serialize, Deserialize, Debug, Derivative, PartialEq, Eq, Hash, Clone, Copy)]
#[derivative(Default)]
pub enum Availability {
    #[derivative(Default)]
    #[serde(rename = "0")]
    InformationNotAvailable,
    #[serde(rename = "1")]
    Available,
    #[serde(rename = "2")]
    NotAvailable,
}

impl Calendar {
    pub fn valid_weekday(&self, date: NaiveDate) -> bool {
        match date.weekday() {
            Weekday::Mon => self.monday,
            Weekday::Tue => self.tuesday,
            Weekday::Wed => self.wednesday,
            Weekday::Thu => self.thursday,
            Weekday::Fri => self.friday,
            Weekday::Sat => self.saturday,
            Weekday::Sun => self.sunday,
        }
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Exception {
    #[serde(rename = "1")]
    Added,
    #[serde(rename = "2")]
    Deleted,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct CalendarDate {
    pub service_id: String,
    #[serde(
        deserialize_with = "deserialize_date",
        serialize_with = "serialize_date"
    )]
    pub date: NaiveDate,
    pub exception_type: Exception,
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
pub struct Stop {
    #[serde(rename = "stop_id")]
    pub id: String,
    #[serde(rename = "stop_code")]
    pub code: Option<String>,
    #[serde(rename = "stop_name")]
    pub name: String,
    #[serde(default, rename = "stop_desc")]
    pub description: String,
    #[serde(default = "default_location_type")]
    pub location_type: LocationType,
    pub parent_station: Option<String>,
    #[serde(deserialize_with = "de_with_optional_float")]
    #[serde(rename = "stop_lon", default)]
    pub longitude: Option<f64>,
    #[serde(deserialize_with = "de_with_optional_float")]
    #[serde(rename = "stop_lat", default)]
    pub latitude: Option<f64>,
    #[serde(rename = "stop_timezone")]
    pub timezone: Option<String>,
    #[serde(deserialize_with = "de_with_empty_default", default)]
    pub wheelchair_boarding: Availability,
}

impl Type for Stop {
    fn object_type(&self) -> ObjectType {
        ObjectType::Stop
    }
}

impl Id for Stop {
    fn id(&self) -> &str {
        &self.id
    }
}

impl fmt::Display for Stop {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct RawStopTime {
    pub trip_id: String,
    /// Arrival time of the stop time.
    /// It's an option since the intermediate stops can have have no arrival
    /// and this arrival needs to be interpolated
    #[serde(
        deserialize_with = "deserialize_optional_time",
        serialize_with = "serialize_optional_time"
    )]
    pub arrival_time: Option<u32>,
    /// Departure time of the stop time.
    /// It's an option since the intermediate stops can have have no departure
    /// and this departure needs to be interpolated
    #[serde(
        deserialize_with = "deserialize_optional_time",
        serialize_with = "serialize_optional_time"
    )]
    pub departure_time: Option<u32>,
    pub stop_id: String,
    pub stop_sequence: u16,
    pub pickup_type: Option<PickupDropOffType>,
    pub drop_off_type: Option<PickupDropOffType>,
}

#[derive(Debug, Default)]
pub struct StopTime {
    pub arrival_time: Option<u32>,
    pub stop: Arc<Stop>,
    pub departure_time: Option<u32>,
    pub pickup_type: Option<PickupDropOffType>,
    pub drop_off_type: Option<PickupDropOffType>,
    pub stop_sequence: u16,
}

impl StopTime {
    pub fn from(stop_time_gtfs: &RawStopTime, stop: Arc<Stop>) -> Self {
        Self {
            arrival_time: stop_time_gtfs.arrival_time,
            departure_time: stop_time_gtfs.departure_time,
            stop,
            pickup_type: stop_time_gtfs.pickup_type,
            drop_off_type: stop_time_gtfs.drop_off_type,
            stop_sequence: stop_time_gtfs.stop_sequence,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Route {
    #[serde(rename = "route_id")]
    pub id: String,
    #[serde(rename = "route_short_name")]
    pub short_name: String,
    #[serde(rename = "route_long_name")]
    pub long_name: String,
    pub route_type: RouteType,
    pub agency_id: Option<String>,
    pub route_order: Option<u32>,
    #[serde(
        deserialize_with = "de_with_optional_color",
        serialize_with = "serialize_optional_color",
        default
    )]
    pub route_color: Option<RGB8>,
    #[serde(
        deserialize_with = "de_with_optional_color",
        serialize_with = "serialize_optional_color",
        default
    )]
    pub route_text_color: Option<RGB8>,
}

impl Type for Route {
    fn object_type(&self) -> ObjectType {
        ObjectType::Route
    }
}

impl Id for Route {
    fn id(&self) -> &str {
        &self.id
    }
}

impl fmt::Display for Route {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.long_name.is_empty() {
            write!(f, "{}", self.long_name)
        } else {
            write!(f, "{}", self.short_name)
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct RawTrip {
    #[serde(rename = "trip_id")]
    pub id: String,
    pub service_id: String,
    pub route_id: String,
    pub shape_id: Option<String>,
    pub trip_headsign: Option<String>, 
}

impl Type for RawTrip {
    fn object_type(&self) -> ObjectType {
        ObjectType::Trip
    }
}

impl Id for RawTrip {
    fn id(&self) -> &str {
        &self.id
    }
}

impl fmt::Display for RawTrip {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "route id: {}, service id: {}",
            self.route_id, self.service_id
        )
    }
}

#[derive(Debug, Default)]
pub struct Trip {
    pub id: String,
    pub service_id: String,
    pub route_id: String,
    pub stop_times: Vec<StopTime>,
    pub shape_id: Option<String>,
    pub trip_headsign: Option<String>, 
}

impl Type for Trip {
    fn object_type(&self) -> ObjectType {
        ObjectType::Trip
    }
}

impl Id for Trip {
    fn id(&self) -> &str {
        &self.id
    }
}

impl fmt::Display for Trip {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "route id: {}, service id: {}",
            self.route_id, self.service_id
        )
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Agency {
    #[serde(rename = "agency_id")]
    pub id: Option<String>,
    #[serde(rename = "agency_name")]
    pub name: String,
    #[serde(rename = "agency_url")]
    pub url: String,
    #[serde(rename = "agency_timezone")]
    pub timezone: String,
    #[serde(rename = "agency_lang")]
    pub lang: Option<String>,
    #[serde(rename = "agency_phone")]
    pub phone: Option<String>,
    #[serde(rename = "agency_fare_url")]
    pub fare_url: Option<String>,
    #[serde(rename = "agency_email")]
    pub email: Option<String>,
}

impl Type for Agency {
    fn object_type(&self) -> ObjectType {
        ObjectType::Agency
    }
}

impl Id for Agency {
    fn id(&self) -> &str {
        match &self.id {
            None => "",
            Some(id) => id,
        }
    }
}

impl fmt::Display for Agency {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Serialize, Deserialize, Default)]
pub struct Shape {
    #[serde(rename = "shape_id")]
    pub id: String,
    #[serde(rename = "shape_pt_lat", default)]
    pub latitude: f64,
    #[serde(rename = "shape_pt_lon", default)]
    pub longitude: f64,
    #[serde(rename = "shape_pt_sequence")]
    pub sequence: usize,
    #[serde(rename = "shape_dist_traveled")]
    pub dist_traveled: Option<f32>,
}

impl Type for Shape {
    fn object_type(&self) -> ObjectType {
        ObjectType::Shape
    }
}

impl Id for Shape {
    fn id(&self) -> &str {
        &self.id
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FareAttribute {
    #[serde(rename = "fare_id")]
    pub id: String,
    pub price: String,
    #[serde(rename = "currency_type")]
    pub currency: String,
    pub payment_method: PaymentMethod,
    pub transfers: Transfers,
    pub agency_id: Option<String>,
    pub transfer_duration: Option<usize>,
}

impl Id for FareAttribute {
    fn id(&self) -> &str {
        &self.id
    }
}

impl Type for FareAttribute {
    fn object_type(&self) -> ObjectType {
        ObjectType::Fare
    }
}

#[derive(Debug, Deserialize, Serialize, Copy, Clone, PartialEq)]
pub enum PaymentMethod {
    #[serde(rename = "0")]
    Aboard,
    #[serde(rename = "1")]
    PreBoarding,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Transfers {
    Unlimited,
    NoTransfer,
    UniqueTransfer,
    TwoTransfers,
    Other(u16),
}

impl<'de> Deserialize<'de> for Transfers {
    fn deserialize<D>(deserializer: D) -> Result<Transfers, D::Error>
    where
        D: Deserializer<'de>,
    {
        let i = Option::<u16>::deserialize(deserializer)?;
        Ok(match i {
            Some(0) => Transfers::NoTransfer,
            Some(1) => Transfers::UniqueTransfer,
            Some(2) => Transfers::TwoTransfers,
            Some(a) => Transfers::Other(a),
            None => Transfers::default(),
        })
    }
}

impl Serialize for Transfers {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Transfers::NoTransfer => serializer.serialize_u16(0),
            Transfers::UniqueTransfer => serializer.serialize_u16(1),
            Transfers::TwoTransfers => serializer.serialize_u16(2),
            Transfers::Other(a) => serializer.serialize_u16(*a),
            Transfers::Unlimited => serializer.serialize_none(),
        }
    }
}

impl Default for Transfers {
    fn default() -> Transfers {
        Transfers::Unlimited
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FeedInfo {
    #[serde(rename = "feed_publisher_name")]
    pub name: String,
    #[serde(rename = "feed_publisher_url")]
    pub url: String,
    #[serde(rename = "feed_lang")]
    pub lang: String,
    #[serde(
        deserialize_with = "deserialize_option_date",
        serialize_with = "serialize_option_date",
        rename = "feed_start_date",
        default
    )]
    pub start_date: Option<NaiveDate>,
    #[serde(
        deserialize_with = "deserialize_option_date",
        serialize_with = "serialize_option_date",
        rename = "feed_end_date",
        default
    )]
    pub end_date: Option<NaiveDate>,
    #[serde(rename = "feed_version")]
    pub version: Option<String>,
}

impl fmt::Display for FeedInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

fn deserialize_date<'de, D>(deserializer: D) -> Result<NaiveDate, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    NaiveDate::parse_from_str(&s, "%Y%m%d").map_err(serde::de::Error::custom)
}

fn serialize_date<'ser, S>(date: &NaiveDate, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(format!("{}{}{}", date.year(), date.month(), date.day()).as_str())
}

fn deserialize_option_date<'de, D>(deserializer: D) -> Result<Option<NaiveDate>, D::Error>
where
    D: Deserializer<'de>,
{
    let s = Option::<String>::deserialize(deserializer)?
        .map(|s| NaiveDate::parse_from_str(&s, "%Y%m%d").map_err(serde::de::Error::custom));
    match s {
        Some(Ok(s)) => Ok(Some(s)),
        Some(Err(e)) => Err(e),
        None => Ok(None),
    }
}

fn serialize_option_date<S>(date: &Option<NaiveDate>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match date {
        None => serializer.serialize_none(),
        Some(d) => {
            serializer.serialize_str(format!("{}{}{}", d.year(), d.month(), d.day()).as_str())
        }
    }
}

fn parse_time_impl(v: Vec<&str>) -> Result<u32, std::num::ParseIntError> {
    Ok(&v[0].parse()? * 3600u32 + &v[1].parse()? * 60u32 + &v[2].parse()?)
}

pub fn parse_time(s: &str) -> Result<u32, crate::Error> {
    let v: Vec<&str> = s.trim_start().split(':').collect();
    if v.len() != 3 {
        Err(crate::Error::InvalidTime(s.to_owned()))
    } else {
        Ok(parse_time_impl(v).map_err(|_| crate::Error::InvalidTime(s.to_owned()))?)
    }
}

fn deserialize_optional_time<'de, D>(deserializer: D) -> Result<Option<u32>, D::Error>
where
    D: Deserializer<'de>,
{
    let s = Option::<String>::deserialize(deserializer)?;

    match s {
        None => Ok(None),
        Some(t) => Ok(Some(parse_time(&t).map_err(de::Error::custom)?)),
    }
}

fn serialize_optional_time<S>(time: &Option<u32>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match time {
        None => serializer.serialize_none(),
        Some(t) => serializer.serialize_str(format!("{}", t).as_str()),
    }
}

fn de_with_optional_float<'de, D>(de: D) -> Result<Option<f64>, D::Error>
where
    D: Deserializer<'de>,
{
    String::deserialize(de).and_then(|s| {
        let s = s.trim();
        if s == "" {
            Ok(None)
        } else {
            s.parse().map(Some).map_err(de::Error::custom)
        }
    })
}

pub fn parse_color(s: &str) -> Result<RGB8, crate::Error> {
    if s.len() != 6 {
        return Err(crate::Error::InvalidColor(s.to_owned()));
    }
    let r =
        u8::from_str_radix(&s[0..2], 16).map_err(|_| crate::Error::InvalidColor(s.to_owned()))?;
    let g =
        u8::from_str_radix(&s[2..4], 16).map_err(|_| crate::Error::InvalidColor(s.to_owned()))?;
    let b =
        u8::from_str_radix(&s[4..6], 16).map_err(|_| crate::Error::InvalidColor(s.to_owned()))?;
    Ok(RGB8::new(r, g, b))
}

fn de_with_optional_color<'de, D>(de: D) -> Result<Option<RGB8>, D::Error>
where
    D: Deserializer<'de>,
{
    String::deserialize(de).and_then(|s| {
        let s = s.trim();
        if s == "" {
            Ok(None)
        } else {
            parse_color(s).map(Some).map_err(de::Error::custom)
        }
    })
}

fn serialize_optional_color<S>(color: &Option<RGB8>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    match color {
        None => serializer.serialize_none(),
        Some(RGB8 { r, g, b }) => {
            serializer.serialize_str(format!("{:02X}{:02X}{:02X}", r, g, b).as_str())
        }
    }
}

pub fn de_with_empty_default<'de, T: Default, D>(de: D) -> Result<T, D::Error>
where
    D: Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(de).map(|opt| opt.unwrap_or_else(Default::default))
}

fn default_location_type() -> LocationType {
    LocationType::StopPoint
}

fn deserialize_bool<'de, D>(deserializer: D) -> Result<bool, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    match &*s {
        "0" => Ok(false),
        "1" => Ok(true),
        &_ => Err(serde::de::Error::custom(format!(
            "Invalid value `{}`, expected 0 or 1",
            s
        ))),
    }
}

fn serialize_bool<'ser, S>(value: &bool, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    if *value {
        serializer.serialize_u8(1)
    } else {
        serializer.serialize_u8(0)
    }
}
