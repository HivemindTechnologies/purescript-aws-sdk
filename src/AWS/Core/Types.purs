module AWS.Core.Types where

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Either (Either)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Object as F
import Prelude (class Show, bind, ($), pure, (>>>), map)

newtype AccessKeyId
  = AccessKeyId String

derive instance ntAccessKeyId :: Newtype AccessKeyId _

derive newtype instance encodeAccessKeyId :: EncodeJson AccessKeyId

newtype Region
  = Region String

derive instance ntRegion :: Newtype Region _

derive newtype instance encodeRegion :: EncodeJson Region

newtype SecretAccessKey
  = SecretAccessKey String

derive instance ntSecretAccessKey :: Newtype SecretAccessKey _

derive newtype instance encodeSecretAccessKey :: EncodeJson SecretAccessKey

newtype SessionToken
  = SessionToken String

derive instance ntSessionToken :: Newtype SessionToken _

derive newtype instance encodeSessionToken :: EncodeJson SessionToken

newtype Arn
  = Arn String

derive instance ntArn :: Newtype Arn _

newtype ExternalId
  = ExternalId String

derive instance ntExternalId :: Newtype ExternalId _

type Instance
  = { id :: InstanceId, "type" :: InstanceType }

newtype InstanceId
  = InstanceId String

derive instance ntInstanceId :: Newtype InstanceId _

derive newtype instance showInstanceId :: Show InstanceId

newtype InstanceType
  = InstanceType String

derive instance ntInstanceType :: Newtype InstanceType _

derive newtype instance showInstanceType :: Show InstanceType

type Credentials
  = { accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , sessionToken :: Maybe SessionToken
    }

newtype Endpoint
  = Endpoint String

derive instance ntEndpoint :: Newtype Endpoint _

derive newtype instance showEndpoint :: Show Endpoint

derive newtype instance encodeEndpoint :: EncodeJson Endpoint

decodeAsMap :: forall r. DecodeJson r => Json -> Either JsonDecodeError (Map.Map String r)
decodeAsMap str = do
  obj <- decodeJson str
  pure $ Map.fromFoldable $ (F.toUnfoldable obj :: Array _)

newtype Tags
  = Tags (Map.Map String String)

derive newtype instance showTags :: Show Tags

derive instance ntTags :: Newtype Tags _

instance tagsDecoder :: DecodeJson Tags where
  decodeJson = decodeAsMap >>> map Tags

type BasicClientPropsR r
  = ( accessKeyId :: Maybe AccessKeyId
    , secretAccessKey :: Maybe SecretAccessKey
    , region :: Maybe Region
    , endpoint :: Maybe Endpoint
    , sessionToken :: Maybe SessionToken
    , credentials :: Maybe Credentials
    | r
    )

type BasicClientProps r
  = Record (BasicClientPropsR r)

type DefaultClientPropsR
  = BasicClientPropsR ()

type DefaultClientProps
  = Record DefaultClientPropsR

data EC2InstanceType
  = T1micro
  | T2nano
  | T2micro
  | T2small
  | T2medium
  | T2large
  | T2xlarge
  | T22xlarge
  | T3nano
  | T3micro
  | T3small
  | T3medium
  | T3large
  | T3xlarge
  | T32xlarge
  | T3anano
  | T3amicro
  | T3asmall
  | T3amedium
  | T3alarge
  | T3axlarge
  | T3a2xlarge
  | T4gnano
  | T4gmicro
  | T4gsmall
  | T4gmedium
  | T4glarge
  | T4gxlarge
  | T4g2xlarge
  | M1small
  | M1medium
  | M1large
  | M1xlarge
  | M3medium
  | M3large
  | M3xlarge
  | M32xlarge
  | M4large
  | M4xlarge
  | M42xlarge
  | M44xlarge
  | M410xlarge
  | M416xlarge
  | M2xlarge
  | M22xlarge
  | M24xlarge
  | Cr18xlarge
  | R3large
  | R3xlarge
  | R32xlarge
  | R34xlarge
  | R38xlarge
  | R4large
  | R4xlarge
  | R42xlarge
  | R44xlarge
  | R48xlarge
  | R416xlarge
  | R5large
  | R5xlarge
  | R52xlarge
  | R54xlarge
  | R58xlarge
  | R512xlarge
  | R516xlarge
  | R524xlarge
  | R5metal
  | R5alarge
  | R5axlarge
  | R5a2xlarge
  | R5a4xlarge
  | R5a8xlarge
  | R5a12xlarge
  | R5a16xlarge
  | R5a24xlarge
  | R5blarge
  | R5bxlarge
  | R5b2xlarge
  | R5b4xlarge
  | R5b8xlarge
  | R5b12xlarge
  | R5b16xlarge
  | R5b24xlarge
  | R5bmetal
  | R5dlarge
  | R5dxlarge
  | R5d2xlarge
  | R5d4xlarge
  | R5d8xlarge
  | R5d12xlarge
  | R5d16xlarge
  | R5d24xlarge
  | R5dmetal
  | R5adlarge
  | R5adxlarge
  | R5ad2xlarge
  | R5ad4xlarge
  | R5ad8xlarge
  | R5ad12xlarge
  | R5ad16xlarge
  | R5ad24xlarge
  | R6gmetal
  | R6gmedium
  | R6glarge
  | R6gxlarge
  | R6g2xlarge
  | R6g4xlarge
  | R6g8xlarge
  | R6g12xlarge
  | R6g16xlarge
  | R6gdmetal
  | R6gdmedium
  | R6gdlarge
  | R6gdxlarge
  | R6gd2xlarge
  | R6gd4xlarge
  | R6gd8xlarge
  | R6gd12xlarge
  | R6gd16xlarge
  | X116xlarge
  | X132xlarge
  | X1exlarge
  | X1e2xlarge
  | X1e4xlarge
  | X1e8xlarge
  | X1e16xlarge
  | X1e32xlarge
  | I2xlarge
  | I22xlarge
  | I24xlarge
  | I28xlarge
  | I3large
  | I3xlarge
  | I32xlarge
  | I34xlarge
  | I38xlarge
  | I316xlarge
  | I3metal
  | I3enlarge
  | I3enxlarge
  | I3en2xlarge
  | I3en3xlarge
  | I3en6xlarge
  | I3en12xlarge
  | I3en24xlarge
  | I3enmetal
  | Hi14xlarge
  | Hs18xlarge
  | C1medium
  | C1xlarge
  | C3large
  | C3xlarge
  | C32xlarge
  | C34xlarge
  | C38xlarge
  | C4large
  | C4xlarge
  | C42xlarge
  | C44xlarge
  | C48xlarge
  | C5large
  | C5xlarge
  | C52xlarge
  | C54xlarge
  | C59xlarge
  | C512xlarge
  | C518xlarge
  | C524xlarge
  | C5metal
  | C5alarge
  | C5axlarge
  | C5a2xlarge
  | C5a4xlarge
  | C5a8xlarge
  | C5a12xlarge
  | C5a16xlarge
  | C5a24xlarge
  | C5adlarge
  | C5adxlarge
  | C5ad2xlarge
  | C5ad4xlarge
  | C5ad8xlarge
  | C5ad12xlarge
  | C5ad16xlarge
  | C5ad24xlarge
  | C5dlarge
  | C5dxlarge
  | C5d2xlarge
  | C5d4xlarge
  | C5d9xlarge
  | C5d12xlarge
  | C5d18xlarge
  | C5d24xlarge
  | C5dmetal
  | C5nlarge
  | C5nxlarge
  | C5n2xlarge
  | C5n4xlarge
  | C5n9xlarge
  | C5n18xlarge
  | C5nmetal
  | C6gmetal
  | C6gmedium
  | C6glarge
  | C6gxlarge
  | C6g2xlarge
  | C6g4xlarge
  | C6g8xlarge
  | C6g12xlarge
  | C6g16xlarge
  | C6gdmetal
  | C6gdmedium
  | C6gdlarge
  | C6gdxlarge
  | C6gd2xlarge
  | C6gd4xlarge
  | C6gd8xlarge
  | C6gd12xlarge
  | C6gd16xlarge
  | C6gnmedium
  | C6gnlarge
  | C6gnxlarge
  | C6gn2xlarge
  | C6gn4xlarge
  | C6gn8xlarge
  | C6gn12xlarge
  | C6gn16xlarge
  | Cc14xlarge
  | Cc28xlarge
  | G22xlarge
  | G28xlarge
  | G34xlarge
  | G38xlarge
  | G316xlarge
  | G3sxlarge
  | G4ad4xlarge
  | G4ad8xlarge
  | G4ad16xlarge
  | G4dnxlarge
  | G4dn2xlarge
  | G4dn4xlarge
  | G4dn8xlarge
  | G4dn12xlarge
  | G4dn16xlarge
  | G4dnmetal
  | Cg14xlarge
  | P2xlarge
  | P28xlarge
  | P216xlarge
  | P32xlarge
  | P38xlarge
  | P316xlarge
  | P3dn24xlarge
  | P4d24xlarge
  | D2xlarge
  | D22xlarge
  | D24xlarge
  | D28xlarge
  | D3xlarge
  | D32xlarge
  | D34xlarge
  | D38xlarge
  | D3enxlarge
  | D3en2xlarge
  | D3en4xlarge
  | D3en6xlarge
  | D3en8xlarge
  | D3en12xlarge
  | F12xlarge
  | F14xlarge
  | F116xlarge
  | M5large
  | M5xlarge
  | M52xlarge
  | M54xlarge
  | M58xlarge
  | M512xlarge
  | M516xlarge
  | M524xlarge
  | M5metal
  | M5alarge
  | M5axlarge
  | M5a2xlarge
  | M5a4xlarge
  | M5a8xlarge
  | M5a12xlarge
  | M5a16xlarge
  | M5a24xlarge
  | M5dlarge
  | M5dxlarge
  | M5d2xlarge
  | M5d4xlarge
  | M5d8xlarge
  | M5d12xlarge
  | M5d16xlarge
  | M5d24xlarge
  | M5dmetal
  | M5adlarge
  | M5adxlarge
  | M5ad2xlarge
  | M5ad4xlarge
  | M5ad8xlarge
  | M5ad12xlarge
  | M5ad16xlarge
  | M5ad24xlarge
  | M5znlarge
  | M5znxlarge
  | M5zn2xlarge
  | M5zn3xlarge
  | M5zn6xlarge
  | M5zn12xlarge
  | M5znmetal
  | H12xlarge
  | H14xlarge
  | H18xlarge
  | H116xlarge
  | Z1dlarge
  | Z1dxlarge
  | Z1d2xlarge
  | Z1d3xlarge
  | Z1d6xlarge
  | Z1d12xlarge
  | Z1dmetal
  | U6tb156xlarge
  | U6tb1112xlarge
  | U9tb1112xlarge
  | U12tb1112xlarge
  | U6tb1metal
  | U9tb1metal
  | U12tb1metal
  | U18tb1metal
  | U24tb1metal
  | A1medium
  | A1large
  | A1xlarge
  | A12xlarge
  | A14xlarge
  | A1metal
  | M5dnlarge
  | M5dnxlarge
  | M5dn2xlarge
  | M5dn4xlarge
  | M5dn8xlarge
  | M5dn12xlarge
  | M5dn16xlarge
  | M5dn24xlarge
  | M5nlarge
  | M5nxlarge
  | M5n2xlarge
  | M5n4xlarge
  | M5n8xlarge
  | M5n12xlarge
  | M5n16xlarge
  | M5n24xlarge
  | R5dnlarge
  | R5dnxlarge
  | R5dn2xlarge
  | R5dn4xlarge
  | R5dn8xlarge
  | R5dn12xlarge
  | R5dn16xlarge
  | R5dn24xlarge
  | R5nlarge
  | R5nxlarge
  | R5n2xlarge
  | R5n4xlarge
  | R5n8xlarge
  | R5n12xlarge
  | R5n16xlarge
  | R5n24xlarge
  | Inf1xlarge
  | Inf12xlarge
  | Inf16xlarge
  | Inf124xlarge
  | M6gmetal
  | M6gmedium
  | M6glarge
  | M6gxlarge
  | M6g2xlarge
  | M6g4xlarge
  | M6g8xlarge
  | M6g12xlarge
  | M6g16xlarge
  | M6gdmetal
  | M6gdmedium
  | M6gdlarge
  | M6gdxlarge
  | M6gd2xlarge
  | M6gd4xlarge
  | M6gd8xlarge
  | M6gd12xlarge
  | M6gd16xlarge
  | Mac1metal
  | X2gdmedium
  | X2gdlarge
  | X2gdxlarge
  | X2gd2xlarge
  | X2gd4xlarge
  | X2gd8xlarge
  | X2gd12xlarge
  | X2gd16xlarge
  | X2gdmetal

instance showEC2InstanceType :: Show EC2InstanceType where
  show T1micro = "t1.micro"
  show T2nano = "t2.nano"
  show T2micro = "t2.micro"
  show T2small = "t2.small"
  show T2medium = "t2.medium"
  show T2large = "t2.large"
  show T2xlarge = "t2.xlarge"
  show T22xlarge = "t2.2xlarge"
  show T3nano = "t3.nano"
  show T3micro = "t3.micro"
  show T3small = "t3.small"
  show T3medium = "t3.medium"
  show T3large = "t3.large"
  show T3xlarge = "t3.xlarge"
  show T32xlarge = "t3.2xlarge"
  show T3anano = "t3a.nano"
  show T3amicro = "t3a.micro"
  show T3asmall = "t3a.small"
  show T3amedium = "t3a.medium"
  show T3alarge = "t3a.large"
  show T3axlarge = "t3a.xlarge"
  show T3a2xlarge = "t3a.2xlarge"
  show T4gnano = "t4g.nano"
  show T4gmicro = "t4g.micro"
  show T4gsmall = "t4g.small"
  show T4gmedium = "t4g.medium"
  show T4glarge = "t4g.large"
  show T4gxlarge = "t4g.xlarge"
  show T4g2xlarge = "t4g.2xlarge"
  show M1small = "m1.small"
  show M1medium = "m1.medium"
  show M1large = "m1.large"
  show M1xlarge = "m1.xlarge"
  show M3medium = "m3.medium"
  show M3large = "m3.large"
  show M3xlarge = "m3.xlarge"
  show M32xlarge = "m3.2xlarge"
  show M4large = "m4.large"
  show M4xlarge = "m4.xlarge"
  show M42xlarge = "m4.2xlarge"
  show M44xlarge = "m4.4xlarge"
  show M410xlarge = "m4.10xlarge"
  show M416xlarge = "m4.16xlarge"
  show M2xlarge = "m2.xlarge"
  show M22xlarge = "m2.2xlarge"
  show M24xlarge = "m2.4xlarge"
  show Cr18xlarge = "cr1.8xlarge"
  show R3large = "r3.large"
  show R3xlarge = "r3.xlarge"
  show R32xlarge = "r3.2xlarge"
  show R34xlarge = "r3.4xlarge"
  show R38xlarge = "r3.8xlarge"
  show R4large = "r4.large"
  show R4xlarge = "r4.xlarge"
  show R42xlarge = "r4.2xlarge"
  show R44xlarge = "r4.4xlarge"
  show R48xlarge = "r4.8xlarge"
  show R416xlarge = "r4.16xlarge"
  show R5large = "r5.large"
  show R5xlarge = "r5.xlarge"
  show R52xlarge = "r5.2xlarge"
  show R54xlarge = "r5.4xlarge"
  show R58xlarge = "r5.8xlarge"
  show R512xlarge = "r5.12xlarge"
  show R516xlarge = "r5.16xlarge"
  show R524xlarge = "r5.24xlarge"
  show R5metal = "r5.metal"
  show R5alarge = "r5a.large"
  show R5axlarge = "r5a.xlarge"
  show R5a2xlarge = "r5a.2xlarge"
  show R5a4xlarge = "r5a.4xlarge"
  show R5a8xlarge = "r5a.8xlarge"
  show R5a12xlarge = "r5a.12xlarge"
  show R5a16xlarge = "r5a.16xlarge"
  show R5a24xlarge = "r5a.24xlarge"
  show R5blarge = "r5b.large"
  show R5bxlarge = "r5b.xlarge"
  show R5b2xlarge = "r5b.2xlarge"
  show R5b4xlarge = "r5b.4xlarge"
  show R5b8xlarge = "r5b.8xlarge"
  show R5b12xlarge = "r5b.12xlarge"
  show R5b16xlarge = "r5b.16xlarge"
  show R5b24xlarge = "r5b.24xlarge"
  show R5bmetal = "r5b.metal"
  show R5dlarge = "r5d.large"
  show R5dxlarge = "r5d.xlarge"
  show R5d2xlarge = "r5d.2xlarge"
  show R5d4xlarge = "r5d.4xlarge"
  show R5d8xlarge = "r5d.8xlarge"
  show R5d12xlarge = "r5d.12xlarge"
  show R5d16xlarge = "r5d.16xlarge"
  show R5d24xlarge = "r5d.24xlarge"
  show R5dmetal = "r5d.metal"
  show R5adlarge = "r5ad.large"
  show R5adxlarge = "r5ad.xlarge"
  show R5ad2xlarge = "r5ad.2xlarge"
  show R5ad4xlarge = "r5ad.4xlarge"
  show R5ad8xlarge = "r5ad.8xlarge"
  show R5ad12xlarge = "r5ad.12xlarge"
  show R5ad16xlarge = "r5ad.16xlarge"
  show R5ad24xlarge = "r5ad.24xlarge"
  show R6gmetal = "r6g.metal"
  show R6gmedium = "r6g.medium"
  show R6glarge = "r6g.large"
  show R6gxlarge = "r6g.xlarge"
  show R6g2xlarge = "r6g.2xlarge"
  show R6g4xlarge = "r6g.4xlarge"
  show R6g8xlarge = "r6g.8xlarge"
  show R6g12xlarge = "r6g.12xlarge"
  show R6g16xlarge = "r6g.16xlarge"
  show R6gdmetal = "r6gd.metal"
  show R6gdmedium = "r6gd.medium"
  show R6gdlarge = "r6gd.large"
  show R6gdxlarge = "r6gd.xlarge"
  show R6gd2xlarge = "r6gd.2xlarge"
  show R6gd4xlarge = "r6gd.4xlarge"
  show R6gd8xlarge = "r6gd.8xlarge"
  show R6gd12xlarge = "r6gd.12xlarge"
  show R6gd16xlarge = "r6gd.16xlarge"
  show X116xlarge = "x1.16xlarge"
  show X132xlarge = "x1.32xlarge"
  show X1exlarge = "x1e.xlarge"
  show X1e2xlarge = "x1e.2xlarge"
  show X1e4xlarge = "x1e.4xlarge"
  show X1e8xlarge = "x1e.8xlarge"
  show X1e16xlarge = "x1e.16xlarge"
  show X1e32xlarge = "x1e.32xlarge"
  show I2xlarge = "i2.xlarge"
  show I22xlarge = "i2.2xlarge"
  show I24xlarge = "i2.4xlarge"
  show I28xlarge = "i2.8xlarge"
  show I3large = "i3.large"
  show I3xlarge = "i3.xlarge"
  show I32xlarge = "i3.2xlarge"
  show I34xlarge = "i3.4xlarge"
  show I38xlarge = "i3.8xlarge"
  show I316xlarge = "i3.16xlarge"
  show I3metal = "i3.metal"
  show I3enlarge = "i3en.large"
  show I3enxlarge = "i3en.xlarge"
  show I3en2xlarge = "i3en.2xlarge"
  show I3en3xlarge = "i3en.3xlarge"
  show I3en6xlarge = "i3en.6xlarge"
  show I3en12xlarge = "i3en.12xlarge"
  show I3en24xlarge = "i3en.24xlarge"
  show I3enmetal = "i3en.metal"
  show Hi14xlarge = "hi1.4xlarge"
  show Hs18xlarge = "hs1.8xlarge"
  show C1medium = "c1.medium"
  show C1xlarge = "c1.xlarge"
  show C3large = "c3.large"
  show C3xlarge = "c3.xlarge"
  show C32xlarge = "c3.2xlarge"
  show C34xlarge = "c3.4xlarge"
  show C38xlarge = "c3.8xlarge"
  show C4large = "c4.large"
  show C4xlarge = "c4.xlarge"
  show C42xlarge = "c4.2xlarge"
  show C44xlarge = "c4.4xlarge"
  show C48xlarge = "c4.8xlarge"
  show C5large = "c5.large"
  show C5xlarge = "c5.xlarge"
  show C52xlarge = "c5.2xlarge"
  show C54xlarge = "c5.4xlarge"
  show C59xlarge = "c5.9xlarge"
  show C512xlarge = "c5.12xlarge"
  show C518xlarge = "c5.18xlarge"
  show C524xlarge = "c5.24xlarge"
  show C5metal = "c5.metal"
  show C5alarge = "c5a.large"
  show C5axlarge = "c5a.xlarge"
  show C5a2xlarge = "c5a.2xlarge"
  show C5a4xlarge = "c5a.4xlarge"
  show C5a8xlarge = "c5a.8xlarge"
  show C5a12xlarge = "c5a.12xlarge"
  show C5a16xlarge = "c5a.16xlarge"
  show C5a24xlarge = "c5a.24xlarge"
  show C5adlarge = "c5ad.large"
  show C5adxlarge = "c5ad.xlarge"
  show C5ad2xlarge = "c5ad.2xlarge"
  show C5ad4xlarge = "c5ad.4xlarge"
  show C5ad8xlarge = "c5ad.8xlarge"
  show C5ad12xlarge = "c5ad.12xlarge"
  show C5ad16xlarge = "c5ad.16xlarge"
  show C5ad24xlarge = "c5ad.24xlarge"
  show C5dlarge = "c5d.large"
  show C5dxlarge = "c5d.xlarge"
  show C5d2xlarge = "c5d.2xlarge"
  show C5d4xlarge = "c5d.4xlarge"
  show C5d9xlarge = "c5d.9xlarge"
  show C5d12xlarge = "c5d.12xlarge"
  show C5d18xlarge = "c5d.18xlarge"
  show C5d24xlarge = "c5d.24xlarge"
  show C5dmetal = "c5d.metal"
  show C5nlarge = "c5n.large"
  show C5nxlarge = "c5n.xlarge"
  show C5n2xlarge = "c5n.2xlarge"
  show C5n4xlarge = "c5n.4xlarge"
  show C5n9xlarge = "c5n.9xlarge"
  show C5n18xlarge = "c5n.18xlarge"
  show C5nmetal = "c5n.metal"
  show C6gmetal = "c6g.metal"
  show C6gmedium = "c6g.medium"
  show C6glarge = "c6g.large"
  show C6gxlarge = "c6g.xlarge"
  show C6g2xlarge = "c6g.2xlarge"
  show C6g4xlarge = "c6g.4xlarge"
  show C6g8xlarge = "c6g.8xlarge"
  show C6g12xlarge = "c6g.12xlarge"
  show C6g16xlarge = "c6g.16xlarge"
  show C6gdmetal = "c6gd.metal"
  show C6gdmedium = "c6gd.medium"
  show C6gdlarge = "c6gd.large"
  show C6gdxlarge = "c6gd.xlarge"
  show C6gd2xlarge = "c6gd.2xlarge"
  show C6gd4xlarge = "c6gd.4xlarge"
  show C6gd8xlarge = "c6gd.8xlarge"
  show C6gd12xlarge = "c6gd.12xlarge"
  show C6gd16xlarge = "c6gd.16xlarge"
  show C6gnmedium = "c6gn.medium"
  show C6gnlarge = "c6gn.large"
  show C6gnxlarge = "c6gn.xlarge"
  show C6gn2xlarge = "c6gn.2xlarge"
  show C6gn4xlarge = "c6gn.4xlarge"
  show C6gn8xlarge = "c6gn.8xlarge"
  show C6gn12xlarge = "c6gn.12xlarge"
  show C6gn16xlarge = "c6gn.16xlarge"
  show Cc14xlarge = "cc1.4xlarge"
  show Cc28xlarge = "cc2.8xlarge"
  show G22xlarge = "g2.2xlarge"
  show G28xlarge = "g2.8xlarge"
  show G34xlarge = "g3.4xlarge"
  show G38xlarge = "g3.8xlarge"
  show G316xlarge = "g3.16xlarge"
  show G3sxlarge = "g3s.xlarge"
  show G4ad4xlarge = "g4ad.4xlarge"
  show G4ad8xlarge = "g4ad.8xlarge"
  show G4ad16xlarge = "g4ad.16xlarge"
  show G4dnxlarge = "g4dn.xlarge"
  show G4dn2xlarge = "g4dn.2xlarge"
  show G4dn4xlarge = "g4dn.4xlarge"
  show G4dn8xlarge = "g4dn.8xlarge"
  show G4dn12xlarge = "g4dn.12xlarge"
  show G4dn16xlarge = "g4dn.16xlarge"
  show G4dnmetal = "g4dn.metal"
  show Cg14xlarge = "cg1.4xlarge"
  show P2xlarge = "p2.xlarge"
  show P28xlarge = "p2.8xlarge"
  show P216xlarge = "p2.16xlarge"
  show P32xlarge = "p3.2xlarge"
  show P38xlarge = "p3.8xlarge"
  show P316xlarge = "p3.16xlarge"
  show P3dn24xlarge = "p3dn.24xlarge"
  show P4d24xlarge = "p4d.24xlarge"
  show D2xlarge = "d2.xlarge"
  show D22xlarge = "d2.2xlarge"
  show D24xlarge = "d2.4xlarge"
  show D28xlarge = "d2.8xlarge"
  show D3xlarge = "d3.xlarge"
  show D32xlarge = "d3.2xlarge"
  show D34xlarge = "d3.4xlarge"
  show D38xlarge = "d3.8xlarge"
  show D3enxlarge = "d3en.xlarge"
  show D3en2xlarge = "d3en.2xlarge"
  show D3en4xlarge = "d3en.4xlarge"
  show D3en6xlarge = "d3en.6xlarge"
  show D3en8xlarge = "d3en.8xlarge"
  show D3en12xlarge = "d3en.12xlarge"
  show F12xlarge = "f1.2xlarge"
  show F14xlarge = "f1.4xlarge"
  show F116xlarge = "f1.16xlarge"
  show M5large = "m5.large"
  show M5xlarge = "m5.xlarge"
  show M52xlarge = "m5.2xlarge"
  show M54xlarge = "m5.4xlarge"
  show M58xlarge = "m5.8xlarge"
  show M512xlarge = "m5.12xlarge"
  show M516xlarge = "m5.16xlarge"
  show M524xlarge = "m5.24xlarge"
  show M5metal = "m5.metal"
  show M5alarge = "m5a.large"
  show M5axlarge = "m5a.xlarge"
  show M5a2xlarge = "m5a.2xlarge"
  show M5a4xlarge = "m5a.4xlarge"
  show M5a8xlarge = "m5a.8xlarge"
  show M5a12xlarge = "m5a.12xlarge"
  show M5a16xlarge = "m5a.16xlarge"
  show M5a24xlarge = "m5a.24xlarge"
  show M5dlarge = "m5d.large"
  show M5dxlarge = "m5d.xlarge"
  show M5d2xlarge = "m5d.2xlarge"
  show M5d4xlarge = "m5d.4xlarge"
  show M5d8xlarge = "m5d.8xlarge"
  show M5d12xlarge = "m5d.12xlarge"
  show M5d16xlarge = "m5d.16xlarge"
  show M5d24xlarge = "m5d.24xlarge"
  show M5dmetal = "m5d.metal"
  show M5adlarge = "m5ad.large"
  show M5adxlarge = "m5ad.xlarge"
  show M5ad2xlarge = "m5ad.2xlarge"
  show M5ad4xlarge = "m5ad.4xlarge"
  show M5ad8xlarge = "m5ad.8xlarge"
  show M5ad12xlarge = "m5ad.12xlarge"
  show M5ad16xlarge = "m5ad.16xlarge"
  show M5ad24xlarge = "m5ad.24xlarge"
  show M5znlarge = "m5zn.large"
  show M5znxlarge = "m5zn.xlarge"
  show M5zn2xlarge = "m5zn.2xlarge"
  show M5zn3xlarge = "m5zn.3xlarge"
  show M5zn6xlarge = "m5zn.6xlarge"
  show M5zn12xlarge = "m5zn.12xlarge"
  show M5znmetal = "m5zn.metal"
  show H12xlarge = "h1.2xlarge"
  show H14xlarge = "h1.4xlarge"
  show H18xlarge = "h1.8xlarge"
  show H116xlarge = "h1.16xlarge"
  show Z1dlarge = "z1d.large"
  show Z1dxlarge = "z1d.xlarge"
  show Z1d2xlarge = "z1d.2xlarge"
  show Z1d3xlarge = "z1d.3xlarge"
  show Z1d6xlarge = "z1d.6xlarge"
  show Z1d12xlarge = "z1d.12xlarge"
  show Z1dmetal = "z1d.metal"
  show U6tb156xlarge = "u-6tb1.56xlarge"
  show U6tb1112xlarge = "u-6tb1.112xlarge"
  show U9tb1112xlarge = "u-9tb1.112xlarge"
  show U12tb1112xlarge = "u-12tb1.112xlarge"
  show U6tb1metal = "u-6tb1.metal"
  show U9tb1metal = "u-9tb1.metal"
  show U12tb1metal = "u-12tb1.metal"
  show U18tb1metal = "u-18tb1.metal"
  show U24tb1metal = "u-24tb1.metal"
  show A1medium = "a1.medium"
  show A1large = "a1.large"
  show A1xlarge = "a1.xlarge"
  show A12xlarge = "a1.2xlarge"
  show A14xlarge = "a1.4xlarge"
  show A1metal = "a1.metal"
  show M5dnlarge = "m5dn.large"
  show M5dnxlarge = "m5dn.xlarge"
  show M5dn2xlarge = "m5dn.2xlarge"
  show M5dn4xlarge = "m5dn.4xlarge"
  show M5dn8xlarge = "m5dn.8xlarge"
  show M5dn12xlarge = "m5dn.12xlarge"
  show M5dn16xlarge = "m5dn.16xlarge"
  show M5dn24xlarge = "m5dn.24xlarge"
  show M5nlarge = "m5n.large"
  show M5nxlarge = "m5n.xlarge"
  show M5n2xlarge = "m5n.2xlarge"
  show M5n4xlarge = "m5n.4xlarge"
  show M5n8xlarge = "m5n.8xlarge"
  show M5n12xlarge = "m5n.12xlarge"
  show M5n16xlarge = "m5n.16xlarge"
  show M5n24xlarge = "m5n.24xlarge"
  show R5dnlarge = "r5dn.large"
  show R5dnxlarge = "r5dn.xlarge"
  show R5dn2xlarge = "r5dn.2xlarge"
  show R5dn4xlarge = "r5dn.4xlarge"
  show R5dn8xlarge = "r5dn.8xlarge"
  show R5dn12xlarge = "r5dn.12xlarge"
  show R5dn16xlarge = "r5dn.16xlarge"
  show R5dn24xlarge = "r5dn.24xlarge"
  show R5nlarge = "r5n.large"
  show R5nxlarge = "r5n.xlarge"
  show R5n2xlarge = "r5n.2xlarge"
  show R5n4xlarge = "r5n.4xlarge"
  show R5n8xlarge = "r5n.8xlarge"
  show R5n12xlarge = "r5n.12xlarge"
  show R5n16xlarge = "r5n.16xlarge"
  show R5n24xlarge = "r5n.24xlarge"
  show Inf1xlarge = "inf1.xlarge"
  show Inf12xlarge = "inf1.2xlarge"
  show Inf16xlarge = "inf1.6xlarge"
  show Inf124xlarge = "inf1.24xlarge"
  show M6gmetal = "m6g.metal"
  show M6gmedium = "m6g.medium"
  show M6glarge = "m6g.large"
  show M6gxlarge = "m6g.xlarge"
  show M6g2xlarge = "m6g.2xlarge"
  show M6g4xlarge = "m6g.4xlarge"
  show M6g8xlarge = "m6g.8xlarge"
  show M6g12xlarge = "m6g.12xlarge"
  show M6g16xlarge = "m6g.16xlarge"
  show M6gdmetal = "m6gd.metal"
  show M6gdmedium = "m6gd.medium"
  show M6gdlarge = "m6gd.large"
  show M6gdxlarge = "m6gd.xlarge"
  show M6gd2xlarge = "m6gd.2xlarge"
  show M6gd4xlarge = "m6gd.4xlarge"
  show M6gd8xlarge = "m6gd.8xlarge"
  show M6gd12xlarge = "m6gd.12xlarge"
  show M6gd16xlarge = "m6gd.16xlarge"
  show Mac1metal = "mac1.metal"
  show X2gdmedium = "x2gd.medium"
  show X2gdlarge = "x2gd.large"
  show X2gdxlarge = "x2gd.xlarge"
  show X2gd2xlarge = "x2gd.2xlarge"
  show X2gd4xlarge = "x2gd.4xlarge"
  show X2gd8xlarge = "x2gd.8xlarge"
  show X2gd12xlarge = "x2gd.12xlarge"
  show X2gd16xlarge = "x2gd.16xlarge"
  show X2gdmetal = "x2gd.metal"

ec2InstanceTypes :: Array String
ec2InstanceTypes =
  [ "t1.micro"
  , "t2.nano"
  , "t2.micro"
  , "t2.small"
  , "t2.medium"
  , "t2.large"
  , "t2.xlarge"
  , "t2.2xlarge"
  , "t3.nano"
  , "t3.micro"
  , "t3.small"
  , "t3.medium"
  , "t3.large"
  , "t3.xlarge"
  , "t3.2xlarge"
  , "t3a.nano"
  , "t3a.micro"
  , "t3a.small"
  , "t3a.medium"
  , "t3a.large"
  , "t3a.xlarge"
  , "t3a.2xlarge"
  , "t4g.nano"
  , "t4g.micro"
  , "t4g.small"
  , "t4g.medium"
  , "t4g.large"
  , "t4g.xlarge"
  , "t4g.2xlarge"
  , "m1.small"
  , "m1.medium"
  , "m1.large"
  , "m1.xlarge"
  , "m3.medium"
  , "m3.large"
  , "m3.xlarge"
  , "m3.2xlarge"
  , "m4.large"
  , "m4.xlarge"
  , "m4.2xlarge"
  , "m4.4xlarge"
  , "m4.10xlarge"
  , "m4.16xlarge"
  , "m2.xlarge"
  , "m2.2xlarge"
  , "m2.4xlarge"
  , "cr1.8xlarge"
  , "r3.large"
  , "r3.xlarge"
  , "r3.2xlarge"
  , "r3.4xlarge"
  , "r3.8xlarge"
  , "r4.large"
  , "r4.xlarge"
  , "r4.2xlarge"
  , "r4.4xlarge"
  , "r4.8xlarge"
  , "r4.16xlarge"
  , "r5.large"
  , "r5.xlarge"
  , "r5.2xlarge"
  , "r5.4xlarge"
  , "r5.8xarge"
  , "r5.12xlarge"
  , "r5.16xlarge"
  , "r5.24xlarge"
  , "r5.metal"
  , "r5a.large"
  , "r5a.xlarge"
  , "r5a.2xlarge"
  , "r5a.4xlarge"
  , "r5a.8xlarge"
  , "r5a.12xlarge"
  , "r5a.16xlarge"
  , "r5a.24xlarge"
  , "r5b.large"
  , "r5b.xlarge"
  , "r5b.2xlarge"
  , "r5b.4xlarge"
  , "r5b.8xlarge"
  , "r5b.12xlarge"
  , "r5b.16xlarge"
  , "r5b.24xlarge"
  , "r5b.metal"
  , "r5d.large"
  , "r5d.xlarge"
  , "r5d.2xlarge"
  , "r5d.4xlarge"
  , "r5d.8xlarge"
  , "r5d.12xlarge"
  , "r5d.16xlarge"
  , "r5d.24xlarge"
  , "r5d.metal"
  , "r5ad.large"
  , "r5ad.xlarge"
  , "r5ad.2xlarge"
  , "r5ad.4xlarge"
  , "r5ad.8xlarge"
  , "r5ad.12xlarge"
  , "r5ad.16xlarge"
  , "r5ad.24xlarge"
  , "r6g.metal"
  , "r6g.medium"
  , "r6g.large"
  , "r6g.xlarge"
  , "r6g.2xlarge"
  , "r6g.4xlarge"
  , "r6g.8xlarge"
  , "r6g.12xlarge"
  , "r6g.16xlarge"
  , "r6gd.metal"
  , "r6gd.medium"
  , "r6gd.large"
  , "r6gd.xlarge"
  , "r6gd.2xlarge"
  , "r6gd.4xlarge"
  , "r6gd.8xlarge"
  , "r6gd.12xlarge"
  , "r6gd.16xlarge"
  , "x1.16xlarge"
  , "x1.32xlarge"
  , "x1e.xlarge"
  , "x1e.2xlarge"
  , "x1e.4xlarge"
  , "x1e.8xlarge"
  , "x1e.16xlarge"
  , "x1e.32xlarge"
  , "i2.xlarge"
  , "i2.2xlarge"
  , "i2.4xlarge"
  , "i2.8xlarge"
  , "i3.large"
  , "i3.xlarge"
  , "i3.2xlarge"
  , "i3.4xlarge"
  , "i3.8xlarge"
  , "i3.16xlarge"
  , "i3.metal"
  , "i3en.large"
  , "i3en.xlarge"
  , "i3en.2xlarge"
  , "i3en.3xlarge"
  , "i3en.6xlarge"
  , "i3en.12xlarge"
  , "i3en.24xlarge"
  , "i3en.metal"
  , "hi1.4xlarge"
  , "hs1.8xlarge"
  , "c1.medium"
  , "c1.xlarge"
  , "c3.large"
  , "c3.xlarge"
  , "c3.2xlarge"
  , "c3.4xlarge"
  , "c3.8xlarge"
  , "c4.large"
  , "c4.xlarge"
  , "c4.2xlarge"
  , "c4.4xlarge"
  , "c4.8xlarge"
  , "c5.large"
  , "c5.xlarge"
  , "c5.2xlarge"
  , "c5.4xlarge"
  , "c5.9xlarge"
  , "c5.12xlarge"
  , "c5.18xlarge"
  , "c5.24xlarge"
  , "c5.metal"
  , "c5a.large"
  , "c5a.xlarge"
  , "c5a.2xlarge"
  , "c5a.4xlarge"
  , "c5a.8xlarge"
  , "c5a.12xlarge"
  , "c5a.16xlarge"
  , "c5a.24xlarge"
  , "c5ad.large"
  , "c5ad.xlarge"
  , "c5ad.2xlarge"
  , "c5ad.4xlarge"
  , "c5ad.8xlarge"
  , "c5ad.12xlarge"
  , "c5ad.16xlarge"
  , "c5ad.24xlarge"
  , "c5d.large"
  , "c5d.xlarge"
  , "c5d.2xlarge"
  , "c5d.4xlarge"
  , "c5d.9xlarge"
  , "c5d.12xlarge"
  , "c5d.18xlarge"
  , "c5d.24xlarge"
  , "c5d.metal"
  , "c5n.large"
  , "c5n.xlarge"
  , "c5n.2xlarge"
  , "c5n.4xlarge"
  , "c5n.9xlarge"
  , "c5n.18xlarge"
  , "c5n.metal"
  , "c6g.metal"
  , "c6g.medium"
  , "c6g.large"
  , "c6g.xlarge"
  , "c6g.2xlarge"
  , "c6g.4xlarge"
  , "c6g.8xlarge"
  , "c6g.12xlarge"
  , "c6g.16xlarge"
  , "c6gd.metal"
  , "c6gd.medium"
  , "c6gd.large"
  , "c6gd.xlarge"
  , "c6gd.2xlarge"
  , "c6gd.4xlarge"
  , "c6gd.8xlarge"
  , "c6gd.12xlarge"
  , "c6gd.16xlarge"
  , "c6gn.medium"
  , "c6gn.large"
  , "c6gn.xlarge"
  , "c6gn.2xlarge"
  , "c6gn.4xlarge"
  , "c6gn.8xlarge"
  , "c6gn.12xlarge"
  , "c6gn.16xlarge"
  , "cc1.4xlarge"
  , "cc2.8xlarge"
  , "g2.2xlarge"
  , "g2.8xlarge"
  , "g3.4xlarge"
  , "g3.8xlarge"
  , "g3.16xlarge"
  , "g3s.xlarge"
  , "g4ad.4xlarge"
  , "g4ad.8xlarge"
  , "g4ad.16xlarge"
  , "g4dn.xlarge"
  , "g4dn.2xlarge"
  , "g4dn.4xlarge"
  , "g4dn.8xlarge"
  , "g4dn.12xlarge"
  , "g4dn.16xlarge"
  , "g4dn.metal"
  , "cg1.4xlarge"
  , "p2.xlarge"
  , "p2.8xlarge"
  , "p2.16xlarge"
  , "p3.2xlarge"
  , "p3.8xlarge"
  , "p3.16xlarge"
  , "p3dn.24xlarge"
  , "p4d.24xlarge"
  , "d2.xlarge"
  , "d2.2xlarge"
  , "d2.4xlarge"
  , "d2.8xlarge"
  , "d3.xlarge"
  , "d3.2xlarge"
  , "d3.4xlarge"
  , "d3.8xlarge"
  , "d3en.xlarge"
  , "d3en.2xlarge"
  , "d3en.4xlarge"
  , "d3en.6xlarge"
  , "d3en.8xlarge"
  , "d3en.12xlarge"
  , "f1.2xlarge"
  , "f1.4xlarge"
  , "f1.16xlarge"
  , "m5.large"
  , "m5.xlarge"
  , "m5.2xlarge"
  , "m5.4xlarge"
  , "m5.8xlarge"
  , "m5.12xlarge"
  , "m5.16xlarge"
  , "m5.24xlarge"
  , "m5.metal"
  , "m5a.large"
  , "m5a.xlarge"
  , "m5a.2xlarge"
  , "m5a.4xlarge"
  , "m5a.8xlarge"
  , "m5a.12xlarge"
  , "m5a.16xlarge"
  , "m5a.24xlarge"
  , "m5d.large"
  , "m5d.xlarge"
  , "m5d.2xlarge"
  , "m5d.4xlarge"
  , "m5d.8xlarge"
  , "m5d.12xlarge"
  , "m5d.16xlarge"
  , "m5d.24xlarge"
  , "m5d.metal"
  , "m5ad.large"
  , "m5ad.xlarge"
  , "m5ad.2xlarge"
  , "m5ad.4xlarge"
  , "m5ad.8xlarge"
  , "m5ad.12xlarge"
  , "m5ad.16xlarge"
  , "m5ad.24xlarge"
  , "m5zn.large"
  , "m5zn.xlarge"
  , "m5zn.2xlarge"
  , "m5zn.3xlarge"
  , "m5zn.6xlarge"
  , "m5zn.12xlarge"
  , "m5zn.metal"
  , "h1.2xlarge"
  , "h1.4xlarge"
  , "h1.8xlarge"
  , "h1.16xlarge"
  , "z1d.large"
  , "z1d.xlarge"
  , "z1d.2xlarge"
  , "z1d.3xlarge"
  , "z1d.6xlarge"
  , "z1d.12xlarge"
  , "z1d.metal"
  , "u-6tb1.56xlarge"
  , "u-6tb1.112xlarge"
  , "u-9tb1.112xlarge"
  , "u-12tb1.112xlarge"
  , "u-6tb1.metal"
  , "u-9tb1.metal"
  , "u-12tb1.metal"
  , "u-18tb1.metal"
  , "u-24tb1.metal"
  , "a1.medium"
  , "a1.large"
  , "a1.xlarge"
  , "a1.2xlarge"
  , "a1.4xlarge"
  , "a1.metal"
  , "m5dn.large"
  , "m5dn.xlarge"
  , "m5dn.2xlarge"
  , "m5dn.4xlarge"
  , "m5dn.8xlarge"
  , "m5dn.12xlarge"
  , "m5dn.16xlarge"
  , "m5dn.24xlarge"
  , "m5n.large"
  , "m5n.xlarge"
  , "m5n.2xlarge"
  , "m5n.4xlarge"
  , "m5n.8xlarge"
  , "m5n.12xlarge"
  , "m5n.16xlarge"
  , "m5n.24xlarge"
  , "r5dn.large"
  , "r5dn.xlarge"
  , "r5dn.2xlarge"
  , "r5dn.4xlarge"
  , "r5dn.8xlarge"
  , "r5dn.12xlarge"
  , "r5dn.16xlarge"
  , "r5dn.24xlarge"
  , "r5n.large"
  , "r5n.xlarge"
  , "r5n.2xlarge"
  , "r5n.4xlarge"
  , "r5n.8xlarge"
  , "r5n.12xlarge"
  , "r5n.16xlarge"
  , "r5n.24xlarge"
  , "inf1.xlarge"
  , "inf1.2xlarge"
  , "inf1.6xlarge"
  , "inf1.24xlarge"
  , "m6g.metal"
  , "m6g.medium"
  , "m6g.large"
  , "m6g.xlarge"
  , "m6g.2xlarge"
  , "m6g.4xlarge"
  , "m6g.8xlarge"
  , "m6g.12xlarge"
  , "m6g.16xlarge"
  , "m6gd.metal"
  , "m6gd.medium"
  , "m6gd.large"
  , "m6gd.xlarge"
  , "m6gd.2xlarge"
  , "m6gd.4xlarge"
  , "m6gd.8xlarge"
  , "m6gd.12xlarge"
  , "m6gd.16xlarge"
  , "mac1.metal"
  , "x2gd.medium"
  , "x2gd.large"
  , "x2gd.xlarge"
  , "x2gd.2xlarge"
  , "x2gd.4xlarge"
  , "x2gd.8xlarge"
  , "x2gd.12xlarge"
  , "x2gd.16xlarge"
  , "x2gd.metal"
  ]
