;;; osdsrv.el --- Server interface to desktop notifications
;;
;;; Commentary:
;;
;; Copyright (C) 2016  Lincoln Clarete <lincoln@clarete.li>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Code:

(require 'dbus)


(defun osdsrv-get-server-information ()
  "Return server info to client."
  '("osdsrv" "https://clarete.li/osdsrv" "0.0.1" "1"))


(defun osdsrv-get-capabilities ()
  "Return server capabilities to the client."
  '("body" "body-hyperlinks" "body-images"))


(defun osdsrv-notify (app-name
                      replaces-id
                      app-icon
                      summary
                      body
                      actions
                      hints
                      expire-timeout)
  "Handle the notifications from external services.

 * APP-NAME: Name of the application that requested the
     notification
 * REPLACES-ID: ID of the message being replaced by the
     notification being received.  Defaults to 0.
 * APP-ICON: Application icon (Format?)
 * SUMMARY: Small description of the notification
 * BODY: Notification content
 * ACTIONS: Actions the user can take when the notification is
     received.
 * HINTS: ?
 * EXPIRE-TIMEOUT: How long the message should be available for
     the user in the main view."
  (message "Notification: %s: %s - %s" app-name summary body))


(defun osdsrv-close-notification (id)
  "Close a notification identified by ID."
  (message "CloseNotification: %s" id))


(defun osdsrv-notification-closed (id reason)
  "Handle the signal `NotificationClosed'.

ID: Identification of the notification that has being closed
REASON: Reason for closing the notification client"
  (message "NotificationClosed: %s" id))


(defun osdsrv-action-involved (id action-key)
  "Handle the signal `ActionInvoked'.

ID: Identification of the notification emitting the action
ACTION-KEY: The Key of the action involved"
  (message "ActionInvoked: %s %s" id action-key))


(defun osdsrv-start ()
  "Register OSDSrv service under `org.freedesktop.Notifications'."
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "Notify" 'osdsrv-notify)
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "GetServerInformation" 'osdsrv-get-server-information)
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "GetCapabilities" 'osdsrv-get-capabilities)
  (dbus-register-method
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "CloseNotification" 'osdsrv-close-notification)

  (dbus-register-signal
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "NotificationClosed" 'osdsrv-notification-closed)
  (dbus-register-signal
   :session "org.freedesktop.Notifications"
   "/org/freedesktop/Notifications" "org.freedesktop.Notifications"
   "ActionInvoked" 'osdsrv-action-involved))


(defun osdsrv-stop ()
  "Unregister OSDSrv D-Bus service."
  (dbus-unregister-service
   :session "org.freedesktop.Notifications"))

(provide 'osdsrv)
;;; osdsrv.el ends here
