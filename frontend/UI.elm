{-
Jasso is Copyright (c) 2021-2024 Homebrew Holdings Pty Ltd.
Contributors retain the copyright for their contributions.

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along
with this program.  If not, see <https://www.gnu.org/licenses/>.
-}

module UI exposing (..)

import Api exposing (..)
import Logo exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Navigation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (id, type_, placeholder, value, style, href)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..), get, post, expectJson, stringPart, multipartBody, expectWhatever)
import Json.Decode exposing (Decoder, string, map2, field, map8, list)
import Task
import Url exposing (..)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Elevation as Elevation
import Material.Fab.Extended as ExtendedFab
import Material.Typography as Typography
import Material.TopAppBar as TopAppBar
import Material.Icon as Icon
import Material.IconButton as IconButton
import Material.List as List
import Material.List.Divider as ListDivider
import Material.List.Item as ListItem
import Material.Menu as Menu
import Material.Drawer.Permanent as PermanentDrawer
import Material.Select as Select
import Material.Select.Item as SelectItem
import Material.Select.Icon as SelectIcon
import Material.Card as Card
import Material.Chip.Action as ActionChip
import Material.ChipSet.Action as ActionChipSet
import Material.TextField as TextField
import Material.Snackbar as Snackbar

const f x = f

type alias Flags = String

init: Flags -> Url -> Key -> (Model, Cmd Msg)
init f _ k = (Model f k Snackbar.initialQueue Nothing, getUserMe MeReturned)

main = Browser.application
  { init = init
  , view = view
  , update = update
  , subscriptions = const Sub.none
  , onUrlRequest = LinkClicked
  , onUrlChange = const Nop
  }

type Status = ViewUser User | EditUser User User | ViewGroup Group | SetUpMFA MFAStatus | AddMFAEmail MFAStatus | AddMFATOTP MFAStatus | AddMFATOTPVerify MFAStatus

type alias State = { u: User, configMenuOpen: Bool, s: List Status }
type alias Model = { flags: Flags, key: Key, q: Snackbar.Queue Msg, st: Maybe State }

type alias LoginReturn = { tag: String, contents: String }

type Fields = GivenName | FamilyName | FullName | Mail

type Msg = Nop | SnackbarClosed Snackbar.MessageId | LinkClicked Browser.UrlRequest | ConfigMenu | MeReturned (Result Http.Error User) | LogoutClicked | UserEditClicked | UserClicked String | UserEditDoneClicked | ChngPassClicked | SetUpMFAClicked | SetUpMFAEmailClicked MFAStatus | SetUpMFATOTPClicked MFAStatus | SetUpMFATOTPVerifyClicked MFAStatus | FieldChanged Fields String | GroupClicked String | UserReturned String (Result Http.Error User) | GroupReturned String (Result Http.Error Group) | BackOrCancelClicked | MFAStatusReturned (Result Http.Error MFAStatus) | ToggleMFAEmailClicked MFAStatus

changeFirst l f t = case l of
  []      -> []
  (x::xs) -> if x == f then t::xs else x::changeFirst xs f t

adjustFN orig from to  = let origw = case String.words orig of
                                       [x] -> if x /= from then [to, x] else [x]
                                       xs  -> xs
                         in  String.join " " <| changeFirst origw from to
adjustFNr orig from to = let origw = case String.words orig of
                                       [x] -> if x /= from then [x, to] else [x]
                                       xs  -> xs
                         in  String.join " " <| List.reverse <| changeFirst (List.reverse <| origw) from to

errorToString : Http.Error -> String
errorToString error =
  case error of
    BadUrl url           -> "The URL " ++ url ++ " was invalid"
    Timeout              -> "Unable to reach the server, try again"
    NetworkError         -> "Unable to reach the server, check your network connection"
    BadStatus 500        -> "The server had a problem, try again later"
    BadStatus 400        -> "Verify your information and try again"
    BadStatus _          -> "Unknown error"
    BadBody errorMessage -> errorMessage

update msg model = case msg of
  LinkClicked urlRequest -> case urlRequest of
    Browser.Internal u -> (model, pushUrl model.key (Url.toString u))
    Browser.External u -> (model, load u)

  SnackbarClosed messageId -> ({model | q = Snackbar.close messageId model.q}, Cmd.none)

  MeReturned (Ok u)  -> ({model | st = Just {u = u, configMenuOpen = False, s = []}}, Cmd.none)
  MeReturned (Err e) -> let m = Snackbar.message (errorToString e) |> Snackbar.setActionButton (Just "Retry") |> Snackbar.setOnActionButtonClick (const Nop)
                        in  ({model | q = Snackbar.addMessage m model.q}, Cmd.none)

  _ -> case model.st of
    Nothing -> (model, getUserMe MeReturned)
    Just st -> case msg of

      GroupReturned _ (Ok g)  -> ({model | st = Just {st | s = ViewGroup g :: st.s}}, Cmd.none)
      GroupReturned g (Err e) -> let m = Snackbar.message (errorToString e) |> Snackbar.setActionButton (Just "Retry") |> Snackbar.setOnActionButtonClick (const <| GroupClicked g)
                                 in  ({model | q = Snackbar.addMessage m model.q}, Cmd.none)

      UserReturned _ (Ok u)  -> ({model | st = Just {st | s = ViewUser u :: st.s}}, Cmd.none)
      UserReturned u (Err e) -> let m = Snackbar.message (errorToString e) |> Snackbar.setActionButton (Just "Retry") |> Snackbar.setOnActionButtonClick (const <| UserClicked u)
                                in  ({model | q = Snackbar.addMessage m model.q}, Cmd.none)

      ToggleMFAEmailClicked mfa -> ({model | st = Just {st | s = List.map (\t -> case t of
        SetUpMFA mfs -> SetUpMFA {mfa | mfaEmailEnabled = not mfa.mfaEmailEnabled}
        _            -> t) <| List.drop 1 st.s}}, Cmd.none) -- FIXME: should update mfastatus via REST API

      MFAStatusReturned (Ok mfa) -> ({model | st = Just {st | s = SetUpMFA mfa :: st.s}}, Cmd.none)
      MFAStatusReturned (Err e)  -> let m = Snackbar.message (errorToString e) |> Snackbar.setActionButton (Just "Retry") |> Snackbar.setOnActionButtonClick (const <| SetUpMFAClicked)
                                    in  ({model | q = Snackbar.addMessage m model.q}, Cmd.none)

      FieldChanged f v -> case (st.s, f) of
        (EditUser u0 u :: r, GivenName)  -> ({model | st = Just {st | s = EditUser u0 {u | givenName  = v, fullName = adjustFN u.fullName u.givenName v} :: r}}, Cmd.none)
        (EditUser u0 u :: r, FamilyName) -> ({model | st = Just {st | s = EditUser u0 {u | familyName = v, fullName = adjustFNr u.fullName u.familyName v} :: r}}, Cmd.none)
        (EditUser u0 u :: r, FullName)   -> ({model | st = Just {st | s = EditUser u0 {u | fullName   = v} :: r}}, Cmd.none)
        (EditUser u0 u :: r, Mail)       -> ({model | st = Just {st | s = EditUser u0 {u | mail       = v} :: r}}, Cmd.none)
        _                                -> (model, Cmd.none)

      LogoutClicked -> (model, load "/logout")

      ChngPassClicked -> (model, Cmd.none)

      UserEditClicked -> case st.s of
        ViewUser u :: _ -> ({model | st = Just {st | s = EditUser u u :: st.s}}, Cmd.none)
        _               -> ({model | st = Just {st | s = EditUser st.u st.u :: st.s}}, Cmd.none)

      UserEditDoneClicked -> case st.s of
        EditUser _ u :: _ -> ({model | st = Just {st | u = u, s = List.drop 1 st.s}}, Cmd.none) -- TODO: if u0 /= u, command should do the update (asynchronously)
        _                 -> (model, Cmd.none)

      BackOrCancelClicked -> ({model | st = Just {st | s = List.drop 1 st.s}}, Cmd.none)

      GroupClicked g -> (model, getGroupByG g (GroupReturned g))

      UserClicked u -> (model, getUserByU u (UserReturned u))

      SetUpMFAClicked -> (model, getUserMfa MFAStatusReturned)

      SetUpMFATOTPClicked mfa -> ({model | st = Just {st | s = AddMFATOTP mfa :: st.s}}, Cmd.none)

      SetUpMFAEmailClicked mfa -> ({model | st = Just {st | s = AddMFAEmail mfa :: st.s}}, Cmd.none)

      SetUpMFATOTPVerifyClicked mfa -> ({model | st = Just {st | s = AddMFATOTPVerify mfa :: st.s}}, Cmd.none) -- TODO: store this somehow so we know it should be used

      ConfigMenu -> ({model | st = Just {st | configMenuOpen = not st.configMenuOpen}}, Cmd.none)

      _ -> (model, Cmd.none)

configMenu open = IconButton.menu (Menu.config |> Menu.setOpen open
                                               |> Menu.setOnClose ConfigMenu)
                                  (ListItem.listItem ListItem.config [text "Menu item"])
                                  [ListItem.listItem ListItem.config [text "Menu item"]]

boilerplate q title body = { title = title, body = [main_ [Typography.typography] <| body, Snackbar.snackbar (Snackbar.config {onClosed = SnackbarClosed}) q, footer [style "position" "fixed", style "bottom" "0", style "right" "0" ] [a [href "https://piglet.engineering/jasso-single-sign-on/"] [logo]]] }

view model = case model.st of
  Nothing -> boilerplate model.q "Login"    <| [h2 [] [text "Loading"]]
  Just st -> boilerplate model.q "Accounts" <| viewmain st

viewmain st = case List.head st.s of
    Nothing -> getMeCard st.u
    Just s  -> case s of
      ViewUser u           -> getUserCard u
      EditUser u0 u        -> getEditCard u <| u0 /= u
      ViewGroup g          -> getGroupCard g
      SetUpMFA mfa         -> getMFACard st.u mfa
      AddMFAEmail mfa      -> getMFAEmailDialog st.u mfa
      AddMFATOTP mfa       -> getMFAEmailCard st.u mfa
      AddMFATOTPVerify mfa -> getMFAEmailCard st.u mfa

mkIconStyle = [style "font-size" "32px", style "vertical-align" "middle", style "padding" "0.5em"]
mkIcon = Icon.icon mkIconStyle

card1 = Card.card (Card.config |> Card.setOutlined True |> Card.setAttributes [style "margin" "2em", style "padding" "0.5em"])

getCard user showGroups actions = [card1
  {blocks =
    (Card.block <| div [style "margin" "0 1em"] [h2 [] [mkIcon "person", text user.uid], h3 [] [text <| user.fullName ++ " <" ++ user.mail ++ ">"]]
    ,if showGroups then [Card.block <| Html.p [] <| getGroups False user.roles] else [])
  ,actions = actions}]

getMeCard user = getCard user True <| Just <| Card.actions
  {buttons = [Card.button (Button.config |> Button.setIcon (Just <| Button.icon "edit") |> Button.setOnClick UserEditClicked) "Edit"
             ,Card.button (Button.config |> Button.setIcon (Just <| Button.icon "key") |> Button.setOnClick ChngPassClicked) "Change Password"
             ,Card.button (Button.config |> Button.setIcon (Just <| Button.icon "enhanced_encryption") |> Button.setOnClick SetUpMFAClicked) "MFA"
             ,Card.button (Button.config |> Button.setIcon (Just <| Button.icon "logout") |> Button.setOnClick LogoutClicked) "Logout"], icons = []}

getUserCard user = getCard user True <| Just <| Card.actions
  {buttons = [Card.button (Button.config |> Button.setIcon (Just <| Button.icon "cancel") |> Button.setOnClick BackOrCancelClicked) "Back"], icons = []}

getGroupCard group = [card1
  {blocks =
    (Card.block <| div [style "margin" "0 1em"] [h2 [] [mkIcon "group", text group.gid], h3 [] [text <| group.description]]
    ,[Card.block <| Html.p [] <| getUsers group.users])
    ,actions = Just <| Card.actions
      {buttons = [Card.button (Button.config |> Button.setIcon (Just <| Button.icon "cancel") |> Button.setOnClick BackOrCancelClicked) "Back"], icons = []}}]

mkTextField name value other = TextField.filled (TextField.config |> TextField.setLabel (Just name)  |> TextField.setValue (Just value) |> TextField.setAttributes ([style "margin" "0.2em"] ++ other))
getEditCard user dirty = [card1
  {blocks =
    (Card.block <| div [style "margin" "0 1em"]
      [h2 [] [text user.uid]
      ,mkTextField "Given Name(s)" user.givenName [style "width" "calc(45% - 0.2em)", onInput (FieldChanged GivenName)]
      ,mkTextField "Family Name(s)" user.familyName [style "width" "calc(45% - 0.2em)", onInput (FieldChanged FamilyName)]
      ,mkTextField "Full Name" user.fullName [style "width" "90%", onInput (FieldChanged FullName)]
      ,mkTextField "Email Address" user.mail [style "width" "90%", onInput (FieldChanged Mail)]],[])
    ,actions = Just <| Card.actions
      {buttons = [Card.button (Button.config |> Button.setIcon (Just <| Button.icon "save") |> Button.setDisabled (not dirty) |> Button.setOnClick UserEditDoneClicked) "Save"
                 ,Card.button (Button.config |> Button.setIcon (Just <| Button.icon "cancel") |> Button.setOnClick BackOrCancelClicked) "Cancel"], icons = []}}]

mkGroupChip editable g = ActionChip.chip (ActionChip.config |> ActionChip.setOnClick (GroupClicked g.rolename) |> ActionChip.setIcon (Just <| ActionChip.icon (if editable then "group_remove" else "group"))) g.rolename
getGroups editable roles = case roles of
  []      -> []
  (g::gs) -> [ActionChipSet.chipSet [] (mkGroupChip editable g) (List.map (mkGroupChip editable) gs)]

mkUserChip u = ActionChip.chip (ActionChip.config |> ActionChip.setOnClick (UserClicked u) |> ActionChip.setIcon (Just <| ActionChip.icon "person")) u
getUsers users = case users of
  []      -> []
  (u::us) -> [ActionChipSet.chipSet [] (mkUserChip u) (List.map mkUserChip us)]

getMFACard user mfa =
  let
    password = ListItem.listItem (ListItem.config |> ListItem.setAttributes [style "pointer-events" "none"])
      [ListItem.graphic [] [mkIcon "password"]
      ,ListItem.text []
        {primary = [text "Password"]
        ,secondary = [text "You cannot disable this method because you must always login with at least a password."]}
      ,ListItem.meta [] [mkIcon ""]]
    authenticator enabled = ListItem.listItem (ListItem.config |> ListItem.setOnClick (SetUpMFATOTPClicked mfa))
      [ListItem.graphic [] [mkIcon "qr_code_scanner"]
      ,ListItem.text []
        {primary = [text "Authenticator app"]
        ,secondary = [text <| if enabled then "Time-based one-time password (TOTP) authentication is enabled." else "Use an authenticator app to verify your identity.  This system uses time-based one-time password (TOTP) authentication.  Be very careful to use a reputable authenticator app.  Most popular password managers also include this facility."]}
      ,ListItem.meta [] [mkIcon "chevron_right"]]
    email enabled = ListItem.listItem (ListItem.config |> ListItem.setOnClick (SetUpMFAEmailClicked mfa))
      [ListItem.graphic [] [mkIcon "email"]
      ,ListItem.text []
        {primary = [text "Email"]
        ,secondary = [text <| if enabled then "Email authentication is enabled." else "Verify your identity by entering a code sent to your email address.  This is less secure than other methods.  Do not enable this method if you use this account to access your email."]}
      ,ListItem.meta [] [mkIcon "chevron_right"]]
    methodList primary secondary methods = case methods of
      []        -> []
      (x :: xs) ->
        [List.subheader [] [ListItem.text [] {primary = [text primary], secondary = [text secondary]}]
        ,List.list (List.config |> List.setAvatarList True |> List.setTwoLine True |> List.setAttributes [style "border" "1px solid lightgray", style "border-radius" "0.5em"]) x (List.concatMap (\y -> [ListDivider.listItem ListDivider.config, y]) xs)]
  in
        [TopAppBar.regular TopAppBar.config [TopAppBar.row [] [TopAppBar.section [TopAppBar.alignStart]
          [IconButton.iconButton (IconButton.config |> IconButton.setOnClick BackOrCancelClicked) (IconButton.icon "arrow_back")
          ,Html.span [TopAppBar.title] [text "Multi-factor Authentication"]]]]
        ,card1
          {blocks =
            (Card.block <| div [style "margin" "0 0.5em"]
              [List.group [style "max-width" "50em", style "margin" "auto"]
                ((methodList "Current multi-factor authentication methods" "These are the configured methods you need to use to verify who you are." <|
                  [password] ++ (if mfa.mfaEmailEnabled then [email True] else []) ++ (if mfa.mfaTotpEnabled then [authenticator True] else []))
              ++ (methodList "Add another authentication method" "Set up extra steps you can use in case your other methods are not available." <|
                  (if not mfa.mfaEmailEnabled then [email False] else []) ++ (if not mfa.mfaTotpEnabled then [authenticator False] else []))
                )],[])
    ,actions = Just <| Card.actions
      {buttons = [Card.button (Button.config |> Button.setIcon (Just <| Button.icon "done") |> Button.setOnClick BackOrCancelClicked) "Done"], icons = []}}]

getMFAEmailCard user mfa =
  [TopAppBar.regular TopAppBar.config [TopAppBar.row [] [TopAppBar.section [TopAppBar.alignStart]
    [IconButton.iconButton (IconButton.config |> IconButton.setOnClick BackOrCancelClicked) (IconButton.icon "arrow_back")
    ,Html.span [TopAppBar.title] [text "Email Authentication"]]]]
  ,card1
    {blocks =
      (Card.block <| div [style "margin" "0 0.5em"]
        [List.list (List.config |> List.setAvatarList True |> List.setAttributes [style "max-width" "50em", style "margin" "auto"])
          (ListItem.listItem (ListItem.config |> ListItem.setOnClick (ToggleMFAEmailClicked mfa))
            [ListItem.graphic [] [mkIcon "email"]
            ,ListItem.text [] {primary = [text <| (if mfa.mfaEmailEnabled then "Dis" else "En") ++ "able Email Authentication"], secondary = [text "Verify your identity by entering a code sent to your email address.  This is less secure than other methods.  Do not enable this method if you use this account to access your email."]}
            ,ListItem.meta [] [mkIcon <| if mfa.mfaEmailEnabled then "toggle_on" else "toggle_off"]
            ]) []],[])
    ,actions = Just <| Card.actions
      {buttons = [Card.button (Button.config |> Button.setIcon (Just <| Button.icon "done") |> Button.setOnClick BackOrCancelClicked) "Done"], icons = []}}]

getMFAEmailDialog user mfa = getMFACard user mfa ++
  [Dialog.confirmation (Dialog.config |> Dialog.setOpen True |> Dialog.setOnClose BackOrCancelClicked)
  {title = if mfa.mfaEmailEnabled then "Remove MFA Method" else "Add MFA Method"
  ,content = [text <| if mfa.mfaEmailEnabled then "Disable Email Authentication" else "Enable Email Authentication"]
  ,actions = [Button.text (Button.config |> Button.setOnClick BackOrCancelClicked |> Button.setIcon (Just <| Button.icon "cancel")) "Cancel", Button.text (Button.config |> Button.setOnClick (ToggleMFAEmailClicked mfa) |> Button.setIcon (Just <| Button.icon "done")) <| if mfa.mfaEmailEnabled then "Disable" else "Enable"]}]
